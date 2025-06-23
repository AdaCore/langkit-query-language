//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.benchmarks;

import com.adacore.lkql_jit.TruffleBenchmark;
import org.openjdk.jmh.annotations.Benchmark;

public class ClosureCreationBenchmark extends TruffleBenchmark {

    @Benchmark
    public void truffle_js() {
        this.context.eval(
                "js",
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

                for (let i = 0; i < 10000; i++) {
                    foo(12)(13)(14)
                }
                        """
            );
    }

    @Benchmark
    public void truffle_lkql() {
        this.context.eval(
                "lkql",
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

                repeat(10000, () => foo(12)(13)(14))
                        """
            );
    }
}
