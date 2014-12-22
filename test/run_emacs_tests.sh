#!/bin/bash

export ENSIME_RUN_AND_EXIT=1

if [ $# -ge 1 ]; then
  emacs --no-init-file --load ./dotemacs_test.el --eval '(ensime-run-one-test "'"$*"'")'
else
  emacs --no-init-file --load ./dotemacs_test.el  --eval '(ensime-run-all-tests)'
fi
