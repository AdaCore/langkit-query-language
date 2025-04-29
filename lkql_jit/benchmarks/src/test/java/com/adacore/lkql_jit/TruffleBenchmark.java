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
@Fork(value = 1, jvmArgsAppend = "-Dgraalvm.locatorDisabled=true")
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
        this.context = Context.create();
    }

    @TearDown
    public void tearDown() {
        this.context.close();
    }
}
