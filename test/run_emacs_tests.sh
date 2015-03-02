#!/bin/bash

if [ "$TRAVIS" = "true" ] ; then
    echo "Starting Xvfb..."
    export DISPLAY=:99
    Xvfb $DISPLAY -screen 0 1024x768x16 &
    sleep 5
fi

export ENSIME_RUN_AND_EXIT=t

if [ -z "$ENSIME_TEST_SERVER_VERSION" ] ; then
    export ENSIME_TEST_SERVER_VERSION=2.11.5
fi

if [ -z "$EMACS" ] ; then
    export EMACS=emacs
fi

if [ $# -ge 1 ]; then
  exec $EMACS --no-init-file --load test/dotemacs_test.el --eval '(ensime-run-one-test "'"$*"'")'
else
  exec $EMACS --no-init-file --load test/dotemacs_test.el  --eval '(ensime-run-all-tests)'
fi
