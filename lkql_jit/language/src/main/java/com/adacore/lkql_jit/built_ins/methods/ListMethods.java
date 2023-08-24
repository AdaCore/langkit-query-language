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

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.functions.UniqueFunction;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLArrayList;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLList;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.util.Arrays;

/**
 * This class contains all built-in methods for the list type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ListMethods extends IterableMethods {

    // ----- Attributes -----

    /** The only instance of the method collection. */
    private static ListMethods instance = null;

    // ----- Constructors -----

    /** Private constructor */
    private ListMethods() {
        super();
    }

    /**
     * Get the only instance of the method collection.
     *
     * @return The instance of the list methods.
     */
    public static ListMethods getInstance() {
        if (instance == null) {
            instance = new ListMethods();
        }
        return instance;
    }

    /**
     * @see CommonMethods#initMethods()
     */
    @Override
    protected void initMethods() {
        super.initMethods();
        this.methods.put(UniqueFunction.NAME, UniqueFunction.getValue());
        this.methods.put(
                "sublist",
                new BuiltInFunctionValue(
                        "sublist",
                        "Return a sublist of `list` from `low_bound` to `high_bound`",
                        new String[] {"list", "low_bound", "high_bound"},
                        new Expr[] {null, null, null},
                        (VirtualFrame frame, FunCall call) -> {
                            var args = frame.getArguments();

                            if (!LKQLTypeSystemGen.isLKQLList(args[0])) {
                                throw LKQLRuntimeException.wrongType(
                                        LKQLTypesHelper.LKQL_LIST,
                                        LKQLTypesHelper.fromJava(args[0]),
                                        call.getArgList().getArgs()[0]);
                            }

                            if (!LKQLTypeSystemGen.isLong(args[1])) {
                                throw LKQLRuntimeException.wrongType(
                                        LKQLTypesHelper.LKQL_INTEGER,
                                        LKQLTypesHelper.fromJava(args[1]),
                                        call.getArgList().getArgs()[1]);
                            }

                            if (!LKQLTypeSystemGen.isLong(args[2])) {
                                throw LKQLRuntimeException.wrongType(
                                        LKQLTypesHelper.LKQL_INTEGER,
                                        LKQLTypesHelper.fromJava(args[2]),
                                        call.getArgList().getArgs()[2]);
                            }

                            LKQLList list = LKQLTypeSystemGen.asLKQLList(args[0]);
                            long lowBound = LKQLTypeSystemGen.asLong(args[1]);
                            long highBound = LKQLTypeSystemGen.asLong(args[2]);

                            if (lowBound < 1) {
                                throw LKQLRuntimeException.invalidIndex((int) lowBound, call);
                            } else if (highBound > list.getContent().length) {
                                throw LKQLRuntimeException.invalidIndex((int) highBound, call);
                            }

                            return new LKQLArrayList(
                                    Arrays.copyOfRange(
                                            list.getContent(),
                                            (int) lowBound - 1,
                                            (int) highBound));
                        }));
    }

    // ----- Override methods -----

    /**
     * @see BuiltInMethods#getType()
     */
    @Override
    public String getType() {
        return LKQLTypesHelper.LKQL_LIST;
    }
}
