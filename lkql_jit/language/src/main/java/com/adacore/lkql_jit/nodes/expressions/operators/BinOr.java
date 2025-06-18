//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.LKQLToBoolean;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Executed;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public abstract class BinOr extends Expr {

    // TODO: Use InlinedConditionProfile when switching to Graal 23

    /** The left operand expression. */
    @Child
    @Executed
    protected Expr left;

    /** The right operand expression. */
    @Child
    protected Expr right;

    BinOr(SourceSection location, Expr left, Expr right) {
        super(location);
        this.left = left;
        this.right = right;
    }

    @Specialization
    protected Object doBoolean(VirtualFrame frame, boolean leftValue) {
        if (leftValue) {
            return leftValue;
        } else {
            return right.executeGeneric(frame);
        }
    }

    @Specialization(replaces = "doBoolean")
    protected Object doGeneric(
        VirtualFrame frame,
        Object leftValue,
        @Cached LKQLToBoolean toBoolean
    ) {
        if (toBoolean.execute(leftValue)) {
            return leftValue;
        } else {
            return right.executeGeneric(frame);
        }
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
