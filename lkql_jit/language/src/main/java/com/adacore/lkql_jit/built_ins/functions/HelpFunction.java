//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class represents "help" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class HelpFunction {

    public static final String NAME = "help";

    /** Get a brand new "help" function value. */
    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given any object, return formatted help for it",
                new String[] {"obj"},
                new Expr[] {null},
                (VirtualFrame frame, FunCall call) -> {
                    // Get the argument
                    Object arg = frame.getArguments()[0];

                    // If the argument is an LKQL value, read the documentation from ir
                    if (LKQLTypeSystemGen.isLKQLValue(arg)) {
                        LKQLValue value = LKQLTypeSystemGen.asLKQLValue(arg);
                        LKQLLanguage.getContext(call)
                                .println(
                                        StringUtils.concat(
                                                value.lkqlProfile(),
                                                "\n",
                                                value.lkqlDocumentation()));
                    }

                    // Return the default empty documentation
                    return LKQLUnit.INSTANCE;
                });
    }
}
