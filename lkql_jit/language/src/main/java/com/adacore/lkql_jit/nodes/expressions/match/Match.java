//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.match;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a pattern matching expression in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class Match extends Expr {

    // ----- Children -----

    /** Expression to match. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    /** Matching arms. */
    @Children
    private final MatchArm[] arms;

    // ----- constructors -----

    /**
     * Create a new match node.
     *
     * @param location The location of the node in the source.
     * @param expr The expression to match.
     * @param arms The matching arms.
     */
    public Match(SourceSection location, Expr expr, MatchArm[] arms) {
        super(location);
        this.expr = expr;
        this.arms = arms;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        // Evaluate the expression as a node
        Object toMatch = this.expr.executeGeneric(frame);

        // For every match arm try to match and return the result
        for (MatchArm arm : this.arms) {
            Object armResult = arm.executeArm(frame, toMatch);
            if (armResult != null) {
                return armResult;
            }
        }

        // If no arm matched, return the unit value
        // Probably want to raise an exception in the future
        return LKQLUnit.INSTANCE;
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
