//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.nodes.expressions.Expr;

/** This class represents the LKQL value of an instantiated built-in method. */
public class BuiltInMethodValue extends BuiltInFunctionValue {

    // ----- Attributes -----

    /** The value of the "this" variable. */
    public final Object thisValue;

    // ----- Constructors -----

    /**
     * Create a new built-in method by calling the built-in function constructor and setting the
     * "this" value.
     */
    public BuiltInMethodValue(
            String name,
            String documentation,
            String[] names,
            Expr[] defaultValues,
            AbstractBuiltInFunctionBody body,
            Object thisValue) {
        super(name, documentation, names, defaultValues, body);
        this.thisValue = thisValue;
    }
}
