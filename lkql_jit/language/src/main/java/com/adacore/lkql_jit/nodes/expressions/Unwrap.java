//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the "unwrap" operation in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class Unwrap extends Expr {

    // ----- Children -----

    /** The expression of the node to unwrap. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    // ----- Constructors -----

    /**
     * Create a new unwrap node with parameters.
     *
     * @param location The location of the node in the source.
     * @param expr The node expression.
     */
    public Unwrap(SourceSection location, Expr expr) {
        super(location);
        this.expr = expr;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Evaluate the node value and test it
        Object nodeValue = this.expr.executeGeneric(frame);
        if (!LKQLTypeSystemGen.isAdaNode(nodeValue)) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.ADA_NODE, LKQLTypesHelper.fromJava(nodeValue), this.expr);
        }

        // Return the node value
        return nodeValue;
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
