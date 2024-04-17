//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.benchmarks;

import com.adacore.lkql_jit.TruffleBenchmark;
import java.util.Arrays;
import org.openjdk.jmh.annotations.Benchmark;

/** This class contains a benchmark to test the list comprehension in difference languages. */
public class ListComprehensionBenchmark extends TruffleBenchmark {

    // ----- Attributes -----

    /** The java array representing the list comprehension generator. */
    private int[] generator = new int[size];

    /** The list comprehension program for LKQL. */
    private String lkqlListComp;

    /** The list comprehension program for JS. */
    private String jsListComp;

    // ----- Benchmark sources -----

    /** Size of the list comprehension to generate */
    private static final int size = 10_000;

    /** The source for the LKQL list comprehension value. */
    private static final String lkqlSource =
            """
    val generator = %s
    val result = [x * 2 for x in generator].to_list
    """;

    /** The fibonacci function in JS */
    private static final String jsSource =
            """
    var generator = %s;
    var result = generator.map(x => x * 2);
    """;

    /** Java recursive fibonacci implementation. */
    private void javaListComp() {
        Arrays.stream(this.generator).map(x -> x * 2).toArray();
    }

    // ----- Benchmark lifecycle -----

    @Override
    public void setup() {
        super.setup();
        for (int i = 0; i < size; i++) {
            this.generator[i] = i;
        }
        final String generatorLiteral = Arrays.toString(this.generator);
        this.lkqlListComp = lkqlSource.formatted(generatorLiteral);
        this.jsListComp = jsSource.formatted(generatorLiteral);
    }

    // ----- Benchmark methods -----

    @Benchmark
    public void truffle_lkql() {
        this.context.eval("lkql", lkqlListComp);
    }

    @Benchmark
    public void truffle_js() {
        this.context.eval("js", jsListComp);
    }

    @Benchmark
    public void native_java() {
        javaListComp();
    }
}
