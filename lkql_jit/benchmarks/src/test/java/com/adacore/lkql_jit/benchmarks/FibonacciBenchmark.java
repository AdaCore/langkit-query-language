//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.benchmarks;

import com.adacore.lkql_jit.TruffleBenchmark;
import org.openjdk.jmh.annotations.Benchmark;

/**
 * This class benchmarks the fibonacci function execution in LKQL and compare it to the Truffle Ruby
 * implementation and native Java code.
 */
public class FibonacciBenchmark extends TruffleBenchmark {

    // ----- Benchmark sources -----

    /** Fibonacci term to compute */
    private static final int term = 20;

    /** The fibonacci function in LKQL */
    private static final String lkqlFibo =
        """
        fun fibo(n) =
            if n <= 0
            then 0
            else if n == 1
                 then 1
                 else fibo(n-1) + fibo(n-2)
        fibo(%d)
        """.formatted(term);

    /** The fibonacci function in JS */
    private static final String jsFibo =
        """
        function fibo(n) {
            if (n == 0) return 0;
            else if (n == 1) return 1;
            return fibo(n-1) + fibo(n-2);
        }
        fibo(%d)
        """.formatted(term);

    /** The fibonacci function in SimpleLanguage */
    private static final String slFibo =
        """
        function fibo(n) {
            if (n < 2) {
                return 1;
            }
            return fibo(n - 1) + fibo(n - 2);
        }
        function main() {
            fibo(%d);
        }
        """.formatted(term);

    /** Java recursive fibonacci implementation. */
    private static int javaFibo(int n) {
        if (n == 0) return 0;
        else if (n == 1) return 1;
        return javaFibo(n - 1) + javaFibo(n - 2);
    }

    // ----- Benchmark methods -----

    @Benchmark
    public void truffle_lkql() {
        this.context.eval("lkql", lkqlFibo);
    }

    @Benchmark
    public void truffle_js() {
        this.context.eval("js", jsFibo);
    }

    @Benchmark
    public void truffle_sl() {
        this.context.eval("sl", slFibo);
    }

    @Benchmark
    public void native_java() {
        javaFibo(term);
    }
}
