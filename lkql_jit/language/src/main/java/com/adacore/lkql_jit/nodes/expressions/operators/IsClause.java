//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the is clause in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@NodeChild(value = "nodeExpr", type = Expr.class)
public abstract class IsClause extends Expr {

    // ----- Children -----

    /** The pattern node to evaluate the "is" clause. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new "is" clause with the parameters.
     *
     * @param location The token location in the source.
     * @param pattern The pattern to execute the is clause.
     */
    protected IsClause(SourceSection location, BasePattern pattern) {
        super(location);
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /**
     * Execute the "is" clause on the provided value.
     *
     * @param object The value to match against the "is" clause pattern.
     * @return A boolean, representing whether the given value has been validated by the pattern.
     */
    @Specialization
    protected boolean executeValue(VirtualFrame frame, Object object) {
        return this.pattern.executeValue(frame, object);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
