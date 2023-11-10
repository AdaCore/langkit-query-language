/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

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
        """
                    .formatted(term);

    /** The fibonacci function in JS */
    private static final String jsFibo =
            """
        function fibo(n) {
            if (n == 0) return 0;
            else if (n == 1) return 1;
            return fibo(n-1) + fibo(n-2);
        }
        fibo(%d)
        """
                    .formatted(term);

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
        """
                    .formatted(term);

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
