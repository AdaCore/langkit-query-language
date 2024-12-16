//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.nodes.expressions.literals.BooleanLiteral;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

/**
 * This class represents the "print" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class PrintFunction {

    // ----- Attributes -----

    /** The name of the function. */
    public static final String NAME = "print";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Built-in print function. Prints whatever is passed as an argument",
                new String[] {"val", "new_line"},
                new Expr[] {null, new BooleanLiteral(null, true)},
                (VirtualFrame frame, FunCall call) -> {
                    // Get the arguments
                    Object toPrint = frame.getArguments()[0];

                    boolean newLine;
                    try {
                        newLine = LKQLTypeSystemGen.expectBoolean(frame.getArguments()[1]);
                    } catch (UnexpectedResultException e) {
                        throw LKQLRuntimeException.wrongType(
                                LKQLTypesHelper.LKQL_BOOLEAN,
                                LKQLTypesHelper.fromJava(e.getResult()),
                                call.getArgList().getArgs()[1]);
                    }

                    // Print the value
                    if (newLine) {
                        LKQLLanguage.getContext(call).println(ObjectUtils.toString(toPrint));
                    } else {
                        LKQLLanguage.getContext(call).print(ObjectUtils.toString(toPrint));
                    }

                    // Return the unit value
                    return LKQLUnit.INSTANCE;
                });
    }
}
