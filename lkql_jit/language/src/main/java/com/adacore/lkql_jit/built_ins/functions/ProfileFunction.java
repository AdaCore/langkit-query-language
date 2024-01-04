//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class represents the "profile" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ProfileFunction {

    // ----- Attributes -----

    /** The name of the function. */
    public static final String NAME = "profile";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given any object, if it is a callable, return its profile as text",
                new String[] {"obj"},
                new Expr[] {null},
                (VirtualFrame frame, FunCall call) -> {
                    // Get the argument
                    Object arg = frame.getArguments()[0];

                    // If the argument is an LKQL value, read the documentation from ir
                    if (LKQLTypeSystemGen.isLKQLValue(arg)) {
                        return ((LKQLValue) arg).lkqlProfile();
                    }

                    // Return the default empty documentation
                    return "";
                });
    }
}
