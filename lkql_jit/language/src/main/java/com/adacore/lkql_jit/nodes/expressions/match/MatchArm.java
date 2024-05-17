//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.match;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents an arm from a match expression in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public class MatchArm extends LKQLNode {

    // ----- Children -----

    /** Pattern to match during the execution. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    /** Result of the arm execution. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    // ----- Constructors -----

    /**
     * Create a new match arm node.
     *
     * @param location The location of the node in the source.
     * @param pattern The pattern of the match arm.
     * @param expr The result of the match arm.
     */
    public MatchArm(SourceSection location, BasePattern pattern, Expr expr) {
        super(location);
        this.pattern = pattern;
        this.expr = expr;
    }

    // ----- Getters -----

    public BasePattern getPattern() {
        return pattern;
    }

    public Expr getExpr() {
        return expr;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public final Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Verify the pattern of the arm and if matched, execute the arm expression.
     *
     * @param toMatch The node to match.
     * @return The result of the arm expression if the pattern is valid, null else.
     */
    public Object executeArm(VirtualFrame frame, Object toMatch) {
        if (this.pattern.executeValue(frame, toMatch)) {
            return this.expr.executeGeneric(frame);
        } else {
            return null;
        }
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
