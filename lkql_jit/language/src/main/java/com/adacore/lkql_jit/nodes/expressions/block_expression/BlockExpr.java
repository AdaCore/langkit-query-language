//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.block_expression;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a block expression in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class BlockExpr extends Expr {

    // ----- Children -----

    /** The body parts of the expression block. */
    @Children
    private final BlockBody[] body;

    /** The expression of the block, to return after the parts execution. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    // ----- Constructors -----

    /**
     * Create a new expression block node.
     *
     * @param location The location of the node in the source.
     * @param bodyParts The block body parts.
     * @param expr The expression to return.
     */
    public BlockExpr(SourceSection location, BlockBody[] bodyParts, Expr expr) {
        super(location);
        this.body = bodyParts;
        this.expr = expr;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     *     (VirtualFrame)
     */
    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        // Execute the declarations
        for (BlockBody bodyPart : this.body) {
            bodyPart.executeGeneric(frame);
        }

        // Return the expression
        return this.expr.executeGeneric(frame);
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
