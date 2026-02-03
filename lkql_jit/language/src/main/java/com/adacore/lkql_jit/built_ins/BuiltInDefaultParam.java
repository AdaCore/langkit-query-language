//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class wraps a Java value in an expression node. It is used to represents default values for
 * built-inn parameters.
 */
public final class BuiltInDefaultParam extends Expr {

    // ----- Attributes -----

    public final Object value;

    // ----- Constructors -----

    public BuiltInDefaultParam(Object value) {
        super(null);
        this.value = value;
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.value;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
            indentLevel,
            new String[] { "value" },
            new Object[] { value }
        );
    }
}
