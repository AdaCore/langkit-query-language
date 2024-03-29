//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class represents the "doc" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class DocFunction {

    // ----- Attributes -----

    /** The name of the function. */
    public static final String NAME = "doc";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given any object, return the documentation associated with it",
                new String[] {"obj"},
                new Expr[] {null},
                (VirtualFrame frame, FunCall call) -> {
                    // Get the argument
                    Object arg = frame.getArguments()[0];

                    // If the argument is an LKQL value, read the documentation from ir
                    if (LKQLTypeSystemGen.isLKQLValue(arg)) {
                        return ((LKQLValue) arg).lkqlDocumentation();
                    }

                    // Return the default empty documentation
                    return "";
                });
    }
}
