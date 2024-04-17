//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents the unpack operation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class Unpack extends Expr {

    // ----- Children -----

    /** The collection to unpack. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    // ----- Constructors -----

    /**
     * Create a new unpack nod.
     *
     * @param location The location of the node in the source
     * @param expr The collection expression to unpack
     */
    public Unpack(SourceLocation location, Expr expr) {
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
        // Execute the collection expression
        Object obj = this.expr.executeGeneric(frame);

        // If the result is a collection, copy it
        if (LKQLTypeSystemGen.isIndexable(obj)) {
            return LKQLTypeSystemGen.asIndexable(obj).getContent();
        } else {
            return obj;
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
