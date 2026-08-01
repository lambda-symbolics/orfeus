(in-package #:orfeus/gui)

(defstruct gui-task
  kind
  function)

(defstruct (gui-queue (:constructor %make-gui-queue))
  (lock (sb-thread:make-mutex :name "Orfeus GUI render queue"))
  (waitqueue (sb-thread:make-waitqueue :name "Orfeus GUI render queue"))
  (tasks '())
  (events '())
  (running 0 :type fixnum)
  worker
  stopping-p)

(defun make-gui-queue ()
  "Create a bounded-worker render queue for the GUI."
  (let ((queue (%make-gui-queue)))
    (setf (gui-queue-worker queue)
          (sb-thread:make-thread (lambda () (gui-queue-worker-loop queue))
                                 :name "Orfeus GUI render worker"))
    queue))

(defun queue-event (queue event)
  (sb-thread:with-mutex ((gui-queue-lock queue))
    (setf (gui-queue-events queue)
          (nconc (gui-queue-events queue) (list event)))))

(defun drain-events (queue)
  (sb-thread:with-mutex ((gui-queue-lock queue))
    (prog1 (gui-queue-events queue)
      (setf (gui-queue-events queue) '()))))

(defun enqueue-gui-task (queue kind function &key replace-kind)
  "Enqueue FUNCTION on QUEUE's single worker.

When REPLACE-KIND is supplied, pending tasks of that kind are discarded. This
coalesces interactive previews without dropping explicit exports."
  (sb-thread:with-mutex ((gui-queue-lock queue))
    (unless (gui-queue-stopping-p queue)
      (when replace-kind
        (setf (gui-queue-tasks queue)
              (delete replace-kind (gui-queue-tasks queue)
                      :key #'gui-task-kind)))
      (setf (gui-queue-tasks queue)
            (nconc (gui-queue-tasks queue)
                   (list (make-gui-task :kind kind :function function))))
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
              (incf (gui-queue-running queue))
              (pop (gui-queue-tasks queue)))))
      (unwind-protect
           (handler-case (funcall (gui-task-function task))
             (error (condition)
               (queue-event queue (list :error (princ-to-string condition)))))
        (sb-thread:with-mutex ((gui-queue-lock queue))
          (decf (gui-queue-running queue)))))))

(defun gui-queue-busy-p (queue)
  "Return true when QUEUE has running or pending work."
  (sb-thread:with-mutex ((gui-queue-lock queue))
    (or (plusp (gui-queue-running queue))
        (not (null (gui-queue-tasks queue))))))

(defun stop-gui-queue (queue)
  "Discard pending work, stop QUEUE's worker, and wait for active work."
  (when queue
    (sb-thread:with-mutex ((gui-queue-lock queue))
      (setf (gui-queue-stopping-p queue) t
            (gui-queue-tasks queue) '())
      (sb-thread:condition-broadcast (gui-queue-waitqueue queue)))
    (let ((worker (gui-queue-worker queue)))
      (when (and worker (sb-thread:thread-alive-p worker)
                 (not (eq worker sb-thread:*current-thread*)))
        (sb-thread:join-thread worker)))
    t))
