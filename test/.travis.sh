#!/usr/bin/bash

export ENSIME_RUN_AND_EXIT=1
export DISPLAY=:99.0
sh -e /etc/init.d/xvfb start
sleep 3
$EMACS --version
$EMACS -d :99 --no-init-file --load test/dotemacs_test.el --eval "(ensime-run-all-tests)"

