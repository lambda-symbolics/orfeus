#!/usr/bin/env bash
# Start the GUI headlessly, screenshot it, and fail on anything printed to
# stderr that looks like an unhandled condition.
#
# The unit suite never builds the window, so it cannot catch a startup failure:
# a layout whose parent did not match its widgets' actual parent once shipped
# because every test passed and a stale screenshot from an earlier run was
# mistaken for proof. Run this before pushing a change to the GUI.
#
#   nix shell nixpkgs#xorg.xvfb nixpkgs#imagemagick --command scripts/gui-smoke.sh
#
# Writes the screenshot to $1, defaulting to a temporary file, and prints the
# path so the layout can be eyeballed as well as checked for crashes.
set -u

shot="${1:-$(mktemp --suffix=.png)}"
display=":${ORFEUS_SMOKE_DISPLAY:-97}"
log="$(mktemp)"

Xvfb "$display" -screen 0 1920x1180x24 >/dev/null 2>&1 &
server=$!
trap 'kill "$server" 2>/dev/null' EXIT
sleep 3

DISPLAY="$display" nix develop --command sbcl --non-interactive \
  --eval '(asdf:load-system "orfeus/gui")' \
  --eval "(sb-thread:make-thread
            (lambda ()
              (sleep 12)
              (uiop:run-program (list \"import\" \"-window\" \"root\" \"$shot\")
                                :ignore-error-status t)
              (sb-ext:quit)))" \
  --eval '(orfeus/gui:run-gui)' >"$log" 2>&1

# A crash in RUN-GUI kills the process before the screenshot thread fires, so
# both signals are checked: the condition report and the missing image.
if grep -qiE "^Unhandled|debugger invoked|unhandled condition" "$log"; then
    echo "gui-smoke: the GUI failed to start" >&2
    grep -iE "^Unhandled|LAYOUT-ERROR|^  [A-Z]" "$log" | head -20 >&2
    exit 1
fi
if [ ! -s "$shot" ]; then
    echo "gui-smoke: no screenshot was produced; the GUI likely died early" >&2
    tail -20 "$log" >&2
    exit 1
fi
echo "gui-smoke: started cleanly, screenshot at $shot"
