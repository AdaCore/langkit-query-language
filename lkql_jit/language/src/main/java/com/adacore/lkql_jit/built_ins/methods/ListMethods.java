//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.createMethod;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.adacore.lkql_jit.built_ins.BuiltInsHolder;
import com.adacore.lkql_jit.built_ins.functions.UniqueFunction;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.util.Arrays;
import java.util.Map;

/**
 * This class contains all built-in methods for the list type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public class ListMethods {

    private static final Map.Entry<String, BuiltInMethodFactory> sublistFunction =
            createMethod(
                    "sublist",
                    "Return a sublist of `list` from `low_bound` to `high_bound`",
                    new String[] {"low_bound", "high_bound"},
                    new Expr[] {null, null},
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

                        return new LKQLList(
                                Arrays.copyOfRange(
                                        list.getContent(), (int) lowBound - 1, (int) highBound));
                    });

    public static final Map<String, BuiltInMethodFactory> methods =
            BuiltInsHolder.combine(
                    Map.ofEntries(
                            Map.entry(
                                    UniqueFunction.NAME,
                                    BuiltInMethodFactory.fromFunctionValue(
                                            UniqueFunction.getValue(), true)),
                            sublistFunction),
                    IterableMethods.methods);
}
