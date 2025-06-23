//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import java.util.concurrent.TimeUnit;
import org.graalvm.polyglot.Context;
import org.openjdk.jmh.annotations.*;

/** This class is the base for all Truffle benchmarks. */
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(
    value = 1,
    jvmArgsAppend = {
        "-Dgraalvm.locatorDisabled=true",
        // NOTE: If you need to debug truffle benchmarks you can add the following options
        // "-Dgraal.Dump=Truffle:1",
        // "-Dgraal.PrintGraph=Network",
        "--add-opens=org.graalvm.truffle/com.oracle.truffle.api.strings=ALL-UNNAMED",
    }
)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
public class TruffleBenchmark {

    // ----- Attributes -----

    /** The polyglot context to run Truffle languages */
    protected Context context;

    // ----- Benchmark lifecycle -----

    @Setup
    public void setup() {
        final var contextBuilder = Context.newBuilder("lkql", "js", "sl");
        configure(contextBuilder);
        context = contextBuilder.build();
        pre();
    }

    /** Provide a hook to pass custom options to the contextBuilder in benchmarks */
    public void configure(Context.Builder contextBuilder) {}

    /** Provide a hook to run arbitrary setup code before running the benchmark */
    public void pre() {}

    @TearDown
    public void tearDown() {
        this.context.close();
    }
}
