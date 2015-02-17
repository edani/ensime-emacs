#!/bin/bash

export ENSIME_RUN_AND_EXIT=1
export ENSIME_TEST_SERVER_VERSION=2.11.5

if [ $# -ge 1 ]; then
  emacs --no-init-file --load ./dotemacs_test.el --eval '(ensime-run-one-test "'"$*"'")'
else
  emacs --no-init-file --load ./dotemacs_test.el  --eval '(ensime-run-all-tests)'
fi
