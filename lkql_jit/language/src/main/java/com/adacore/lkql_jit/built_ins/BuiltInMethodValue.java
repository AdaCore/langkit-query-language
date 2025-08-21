//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;

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
        String documentation,
        String[] names,
        String[] defaultValues,
        FunctionRootNode functionRootNode,
        Object thisValue
    ) {
        super(documentation, names, defaultValues, functionRootNode);
        this.thisValue = thisValue;
    }
}
