//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class represents the "units" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class UnitsFunction {

    // ----- Attributes -----

    /** The name of the built-in. */
    public static final String NAME = "units";

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Return an iterator on all units",
                new String[] {},
                new Expr[] {},
                (VirtualFrame frame, FunCall call) ->
                        new LKQLList(LKQLLanguage.getContext(call).getAllUnits()));
    }
}
