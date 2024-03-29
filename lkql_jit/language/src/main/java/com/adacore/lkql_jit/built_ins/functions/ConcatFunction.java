//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLList;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class represents the "concat" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ConcatFunction {

    // ----- Attributes -----

    /** The name of the function. */
    public static final String NAME = "concat";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a list of lists or strings, return a concatenated list or string",
                new String[] {"lists"},
                new Expr[] {null},
                (VirtualFrame frame, FunCall call) -> {

                    // Get the argument
                    Object lists = frame.getArguments()[0];

                    // Check the type of the argument
                    if (!LKQLTypeSystemGen.isLKQLList(lists)) {
                        throw LKQLRuntimeException.wrongType(
                                LKQLTypesHelper.LKQL_LIST,
                                LKQLTypesHelper.fromJava(lists),
                                call.getArgList().getArgs()[0]);
                    }

                    // Cast the argument to list
                    LKQLList listValue = LKQLTypeSystemGen.asLKQLList(lists);

                    // If the list is not empty
                    if (listValue.size() > 0) {
                        final Object firstItem = listValue.get(0);

                        // If the first value is a string look for strings in the list
                        if (LKQLTypeSystemGen.isString(firstItem)) {
                            // Create a string builder and add all strings in the list
                            String result = LKQLTypeSystemGen.asString(firstItem);
                            for (int i = 1; i < listValue.size(); i++) {
                                final Object item = listValue.get(i);
                                if (!LKQLTypeSystemGen.isString(item)) {
                                    throw LKQLRuntimeException.wrongType(
                                            LKQLTypesHelper.LKQL_STRING,
                                            LKQLTypesHelper.fromJava(item),
                                            call.getArgList().getArgs()[0]);
                                }
                                result =
                                        StringUtils.concat(
                                                result, LKQLTypeSystemGen.asString(item));
                            }

                            // Return the result
                            return result;
                        }

                        // If the first item is a list look for lists in the list
                        if (LKQLTypeSystemGen.isLKQLList(firstItem)) {
                            // Create a result array and add all list of the argument
                            Object[] result = LKQLTypeSystemGen.asLKQLList(firstItem).getContent();
                            for (int i = 1; i < listValue.size(); i++) {
                                final Object item = listValue.get(i);
                                if (!LKQLTypeSystemGen.isLKQLList(item)) {
                                    throw LKQLRuntimeException.wrongType(
                                            LKQLTypesHelper.LKQL_LIST,
                                            LKQLTypesHelper.fromJava(item),
                                            call.getArgList().getArgs()[0]);
                                }
                                result =
                                        ArrayUtils.concat(
                                                result,
                                                LKQLTypeSystemGen.asLKQLList(item).getContent());
                            }
                            return new LKQLList(result);
                        }

                        // Else there is an error
                        throw LKQLRuntimeException.wrongType(
                                LKQLTypesHelper.typeUnion(
                                        LKQLTypesHelper.LKQL_LIST, LKQLTypesHelper.LKQL_STRING),
                                LKQLTypesHelper.fromJava(firstItem),
                                call.getArgList().getArgs()[0]);
                    }

                    // If the list is empty just return an empty list
                    else {
                        return new LKQLList(new Object[0]);
                    }
                });
    }
}
