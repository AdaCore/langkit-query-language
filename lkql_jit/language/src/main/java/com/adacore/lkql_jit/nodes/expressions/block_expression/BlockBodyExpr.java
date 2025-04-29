//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.block_expression;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents an expression part in a block expression.
 *
 * @author Hugo GUERRIER
 */
public final class BlockBodyExpr extends BlockBody {

    // ----- Children -----

    /** The expression of the body part */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    // ----- Constructors -----

    /**
     * Create a new expression block body part.
     *
     * @param location The location of the node in the source.
     * @param expr The expression of the block body part.
     */
    public BlockBodyExpr(SourceSection location, Expr expr) {
        super(location);
        this.expr = expr;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBody#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        Object result = this.expr.executeGeneric(frame);

        // Verify if the expression result is nullish
        if (LKQLTypeSystemGen.isNullish(result)) {
            return result;
        } else {
            throw LKQLRuntimeException.ignoredExpressionReturn(this);
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
