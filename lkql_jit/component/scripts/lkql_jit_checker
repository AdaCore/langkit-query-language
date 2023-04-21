#!/bin/bash

if [ -z ${LKQL_JIT_HOME+x} ]; then
  export LKQL_JIT_HOME="$GRAAL_HOME/languages/lkql";
fi

"$GRAAL_HOME/bin/java" \
    -cp "$GRAAL_HOME/lib/truffle/truffle-api.jar:$LKQL_JIT_HOME/lkql_jit.jar:$LKQL_JIT_HOME/lkql_jit_checker.jar" \
    -Djava.library.path="$LD_LIBRARY_PATH" \
    -Dtruffle.class.path.append="$LKQL_JIT_HOME/lkql_jit.jar" \
    com.adacore.lkql_jit.LKQLChecker "$@"
