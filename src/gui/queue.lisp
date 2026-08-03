(in-package #:orfeus/gui)

(defstruct gui-task
  kind
  generation
  function)

(defstruct (gui-queue (:constructor %make-gui-queue))
  (lock (sb-thread:make-mutex :name "Orfeus GUI render queue"))
  (waitqueue (sb-thread:make-waitqueue :name "Orfeus GUI render queue"))
  (tasks '())
  (events '())
  (running 0 :type fixnum)
  (running-kinds '())
  (workers '())
  stopping-p)

(defun make-gui-queue (&key (workers 1) (name "Orfeus GUI render worker"))
  "Create a render queue serviced by WORKERS bounded worker threads."
  (check-type workers (integer 1 *))
  (let ((queue (%make-gui-queue)))
    (setf (gui-queue-workers queue)
          (loop for index below workers
                collect (sb-thread:make-thread
                         (lambda () (gui-queue-worker-loop queue))
                         :name (if (= workers 1)
                                   name
                                   (format nil "~A ~D" name (1+ index))))))
    queue))

(defun queue-event (queue event)
  (sb-thread:with-mutex ((gui-queue-lock queue))
    (setf (gui-queue-events queue)
          (nconc (gui-queue-events queue) (list event)))))

(defun drain-events (queue)
  (sb-thread:with-mutex ((gui-queue-lock queue))
    (prog1 (gui-queue-events queue)
      (setf (gui-queue-events queue) '()))))

(defun enqueue-gui-task (queue kind function &key replace-kind front-p generation)
  "Enqueue FUNCTION on QUEUE.

When REPLACE-KIND is supplied, pending tasks of that kind are discarded. This
coalesces interactive previews without dropping explicit exports. FRONT-P puts
latency-sensitive work ahead of ordinary background tasks."
  (sb-thread:with-mutex ((gui-queue-lock queue))
    (unless (gui-queue-stopping-p queue)
      (when replace-kind
        (setf (gui-queue-tasks queue)
              (delete replace-kind (gui-queue-tasks queue)
                      :key #'gui-task-kind)))
      (let ((task (make-gui-task :kind kind :generation generation :function function)))
        (setf (gui-queue-tasks queue)
              (if front-p
                  (cons task (gui-queue-tasks queue))
                  (nconc (gui-queue-tasks queue) (list task)))))
      (sb-thread:condition-notify (gui-queue-waitqueue queue))
      t)))

(defun discard-gui-tasks (queue &optional kind)
  "Discard pending QUEUE tasks, optionally only those whose kind is KIND."
  (sb-thread:with-mutex ((gui-queue-lock queue))
    (setf (gui-queue-tasks queue)
          (if kind
              (delete kind (gui-queue-tasks queue) :key #'gui-task-kind)
              '())))
  t)

(defun gui-queue-worker-loop (queue)
  (loop
    (let ((task
            (sb-thread:with-mutex ((gui-queue-lock queue))
              (loop while (and (null (gui-queue-tasks queue))
                               (not (gui-queue-stopping-p queue)))
                    do (sb-thread:condition-wait (gui-queue-waitqueue queue)
                                                 (gui-queue-lock queue)))
              (when (and (gui-queue-stopping-p queue)
                         (null (gui-queue-tasks queue)))
                (return-from gui-queue-worker-loop nil))
              (let ((task (pop (gui-queue-tasks queue))))
                (incf (gui-queue-running queue))
                (push task (gui-queue-running-kinds queue))
                task))))
      (unwind-protect
           (handler-case (funcall (gui-task-function task))
             (error (condition)
               (queue-event queue (list :error (princ-to-string condition)))))
        (sb-thread:with-mutex ((gui-queue-lock queue))
          (decf (gui-queue-running queue))
          (setf (gui-queue-running-kinds queue)
                (delete task (gui-queue-running-kinds queue)
                        :count 1 :test #'eq)))))))

(defun gui-queue-busy-p (queue)
  "Return true when QUEUE has running or pending work."
  (sb-thread:with-mutex ((gui-queue-lock queue))
    (or (plusp (gui-queue-running queue))
        (not (null (gui-queue-tasks queue))))))

(defun gui-queue-load (queue &key exclude-kinds include-kinds generation)
  "Return pending plus running work, optionally filtered by kind and generation."
  (flet ((counted-p (task)
           (and (or (null include-kinds)
                    (member (gui-task-kind task) include-kinds))
                (not (member (gui-task-kind task) exclude-kinds))
                (or (null generation)
                    (eql generation (gui-task-generation task))))))
    (sb-thread:with-mutex ((gui-queue-lock queue))
      (+ (count-if #'counted-p (gui-queue-running-kinds queue))
         (count-if #'counted-p (gui-queue-tasks queue))))))

(defun stop-gui-queue (queue)
  "Discard pending work, stop QUEUE's workers, and wait for active work."
  (when queue
    (sb-thread:with-mutex ((gui-queue-lock queue))
      (setf (gui-queue-stopping-p queue) t
            (gui-queue-tasks queue) '())
      (sb-thread:condition-broadcast (gui-queue-waitqueue queue)))
    (dolist (worker (gui-queue-workers queue))
      (when (and worker (sb-thread:thread-alive-p worker)
                 (not (eq worker sb-thread:*current-thread*)))
        (sb-thread:join-thread worker)))
    t))
