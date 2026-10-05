//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.patterns.Pattern;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents an "is" clause. */
@NodeChild(value = "nodeExpr", type = Expr.class)
public abstract class IsClause extends Expr {

    // ----- Children -----

    /** The pattern node to evaluate the "is" clause. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Pattern pattern;

    // ----- Constructors -----

    /**
     * Create a new "is" clause with the parameters.
     *
     * @param location The node location in the source.
     * @param pattern The pattern to match the value against.
     */
    protected IsClause(SourceSection location, Pattern pattern) {
        super(location);
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /** Execute the "is" clause on the provided value. */
    @Specialization
    protected boolean doGeneric(VirtualFrame frame, Object object) {
        return this.pattern.executeValue(frame, object);
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
