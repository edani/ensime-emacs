#!/bin/bash

# tests must be run from the parent of the test directory
cd "`dirname $0`/../"

if [ "$CONTINUOUS_INTEGRATION" = "true" ] ; then
    echo "Starting Xvfb..."
    export DISPLAY=:99
    Xvfb $DISPLAY -screen 0 1024x768x16 > /dev/null 2>&1 &
    sleep 5
fi

# set to 'nil' when writing tests, it'll leave emacs open for you to
# interactively inspect the environment
if [ -z "$ENSIME_RUN_AND_EXIT" ] ; then
   export ENSIME_RUN_AND_EXIT=t
fi

export ENSIME_SKIP_JRE_INDEX=t

if [ -z "$SCALA_VERSION" ] ; then
    export SCALA_VERSION=2.11.6
fi

if [ -z "$EMACS" ] ; then
    export EMACS=`which emacs`
fi

if [ -z "$JDK_HOME" ] ; then
    if [ -n "$JAVA_HOME" ] ; then
        export JDK_HOME=$JAVA_HOME
    elif [ -x "/usr/libexec/java_home" ] ; then
        export JDK_HOME=`/usr/libexec/java_home`
    else
        JAVAC=`which javac`
        export JDK_HOME=$(readlink -f $JAVAC | sed "s:bin/javac::")
    fi
fi
export JAVA_HOME="$JDK_HOME/jre"

if [ $# -ge 1 ]; then
  exec $EMACS --no-init-file --load test/dotemacs_test.el --eval '(ensime-run-one-test "'"$*"'")'
else
  exec $EMACS --no-init-file --load test/dotemacs_test.el  --eval '(ensime-run-all-tests)'
fi
