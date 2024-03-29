//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class represents the "img" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ImgFunction {

    // ----- Attributes -----

    /** The name of the built-in. */
    public static final String NAME = "img";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Return a string representation of an object",
                new String[] {"val"},
                new Expr[] {null},
                (VirtualFrame frame, FunCall call) -> {
                    // Return the string representation of the argument
                    if (frame.getArguments()[0] instanceof String s) {
                        return StringUtils.toRepr(s);
                    } else {
                        return ObjectUtils.toString(frame.getArguments()[0]);
                    }
                });
    }
}
