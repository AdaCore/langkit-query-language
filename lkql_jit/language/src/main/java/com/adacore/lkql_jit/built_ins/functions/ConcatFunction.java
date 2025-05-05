//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.nodes.utils.ConcatenationNode;
import com.adacore.lkql_jit.nodes.utils.ConcatenationNodeGen;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
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

    /** Util node to concatenate values. */
    public static final ConcatenationNode concatNode = ConcatenationNodeGen.create();

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a list, return the result of the concatenation of all its elements",
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
                    LKQLList list = LKQLTypeSystemGen.asLKQLList(lists);

                    // Execute the concatenation on the argument list
                    if (list.size() > 1) {
                        Object res = concatNode.execute(list.get(0), list.get(1), call);
                        for (int i = 2; i < list.size(); i++) {
                            res = concatNode.execute(res, list.get(i), call);
                        }
                        return res;
                    } else if (list.size() == 1) {
                        return list.get(0);
                    } else {
                        return new LKQLList(new Object[0]);
                    }
                });
    }
}
