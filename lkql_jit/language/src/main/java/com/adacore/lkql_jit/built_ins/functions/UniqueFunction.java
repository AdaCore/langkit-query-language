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
import com.adacore.lkql_jit.runtime.values.interfaces.Indexable;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class represents the "unique" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class UniqueFunction {

    // ----- Attributes -----

    /** The name of the function. */
    public static final String NAME = "unique";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given collection, remove all identical elements in order to have only one instance"
                        + " of each",
                new String[] {"indexable"},
                new Expr[] {null},
                (VirtualFrame frame, FunCall call) -> {
                    // Get the argument
                    Object indexableObject = frame.getArguments()[0];

                    // Verify the argument type
                    if (!LKQLTypeSystemGen.isIndexable(indexableObject)) {
                        throw LKQLRuntimeException.wrongType(
                                LKQLTypesHelper.LKQL_LIST,
                                LKQLTypesHelper.fromJava(indexableObject),
                                call.getArgList().getArgs()[0]);
                    }

                    // Cast the argument
                    Indexable indexable = LKQLTypeSystemGen.asIndexable(indexableObject);

                    // Return the result list
                    return new LKQLList(
                            ArrayUtils.unique(indexable.getContent()).toArray(new Object[0]));
                });
    }
}
