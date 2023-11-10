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
