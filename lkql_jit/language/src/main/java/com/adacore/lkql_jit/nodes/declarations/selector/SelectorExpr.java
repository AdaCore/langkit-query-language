//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations.selector;

import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents an expression in the right part of a selector arm.
 *
 * @author Hugo GUERRIER
 */
public final class SelectorExpr extends LKQLNode {

    // ----- Attributes -----

    /** The mode of the selector expression. */
    private final Mode mode;

    // ----- Children -----

    /** The expression of the selector expression. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    public final boolean hasUnpack;

    // ----- Constructors -----

    /**
     * Create a new selector expression node.
     *
     * @param location The location of the node in the source.
     * @param mode The mode of the expression.
     * @param expr The expression.
     * @param hasUnpack
     */
    public SelectorExpr(SourceSection location, Mode mode, Expr expr, boolean hasUnpack) {
        super(location);
        this.mode = mode;
        this.expr = expr;
        this.hasUnpack = hasUnpack;
    }

    // ----- Getters -----

    public Mode getMode() {
        return this.mode;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.expr.executeGeneric(frame);
    }

    // ----- Override methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"mode"}, new Object[] {this.mode});
    }

    // ----- Inner classes -----

    /** This enum represents the mode of the selector expression. */
    public enum Mode {
        /** Default mode, return the expression. */
        DEFAULT,

        /** Recursive mode, return and recurse on the result. */
        REC,

        /** Skip mode, recurse on the result but don't return it. */
        SKIP
    }
}
