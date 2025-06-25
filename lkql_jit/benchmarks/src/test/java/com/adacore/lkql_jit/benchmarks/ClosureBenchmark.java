//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.benchmarks;

import com.adacore.lkql_jit.TruffleBenchmark;
import org.openjdk.jmh.annotations.Benchmark;

public class ClosureBenchmark extends TruffleBenchmark {

    private static final String lkqlClosure =
        """
        fun foo(v) = {
            fun bar(w) = {
                fun baz(x) = {
                    v + w + x
                };
                baz
            };
            bar
        }

        val clos = foo(12)(13)
        fun r() = clos(14)
        repeat(1000000, r)""";

    private static final String jsClosure =
        """
        function foo(v) {
            function bar(w) {
                function baz(x) {
                    return v + w + x;
                }
                return baz;
            }
            return bar;
        }

        var clos = foo(12)(13)

        function r() {
            return clos(14)
        }

        for (let i = 0; i < 1000000; i++) {
            r()
        }""";

    // ----- Benchmark methods -----

    @Benchmark
    public void truffle_lkql() {
        this.context.eval("lkql", lkqlClosure);
    }

    @Benchmark
    public void truffle_js() {
        this.context.eval("js", jsClosure);
    }
}
