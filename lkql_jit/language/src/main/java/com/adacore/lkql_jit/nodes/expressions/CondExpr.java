//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Executed;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public abstract class CondExpr extends Expr {

    // ----- Children -----

    /** The condition of the branching. */
    @Child
    @Executed
    @SuppressWarnings("FieldMayBeFinal")
    protected Expr condition;

    /** The consequence of the branching. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected Expr consequence;

    /** The alternative of the branching. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected Expr alternative;

    public CondExpr(SourceSection location, Expr condition, Expr consequence, Expr alternative) {
        super(location);
        this.condition = condition;
        this.consequence = consequence;
        this.alternative = alternative;
    }

    // ----- Execution methods -----

    @Specialization
    protected Object doBoolean(VirtualFrame frame, boolean conditionResult) {
        if (conditionResult) {
            return consequence.executeGeneric(frame);
        } else if (this.alternative != null) {
            return alternative.executeGeneric(frame);
        } else {
            return true;
        }
    }

    @Specialization(replaces = "doBoolean")
    protected Object doObject(
        VirtualFrame frame,
        Object conditionResult,
        @Cached LKQLToBoolean toBooleanNode
    ) {
        return doBoolean(frame, toBooleanNode.execute(conditionResult));
    }

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
