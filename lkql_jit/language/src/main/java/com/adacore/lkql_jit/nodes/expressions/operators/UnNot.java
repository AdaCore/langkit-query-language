//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.LKQLToBoolean;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a logic unary negation. */
@NodeChild(value = "arg", type = Expr.class)
public abstract class UnNot extends UnOp {

    // ----- Constructors -----

    /**
     * Create a logic unary negation node.
     *
     * @param location The location of the node in the source.
     */
    protected UnNot(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    @Specialization
    protected boolean negateBoolean(boolean arg) {
        return !arg;
    }

    @Specialization(replaces = "negateBoolean")
    protected boolean notBoolean(Object arg, @Cached LKQLToBoolean toBoolean) {
        return !toBoolean.execute(arg);
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
