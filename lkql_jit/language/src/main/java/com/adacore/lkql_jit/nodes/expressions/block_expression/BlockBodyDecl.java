//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.block_expression;

import com.adacore.lkql_jit.nodes.declarations.Declaration;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents a declaration part of a block expression.
 *
 * @author Hugo GUERRIER
 */
public final class BlockBodyDecl extends BlockBody {

    // ----- Children -----

    /** The declaration of the body part. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Declaration decl;

    // ----- Constructors -----

    /**
     * Create a new block body declaration part.
     *
     * @param location The location of the node in the source.
     * @param decl The declaration of the body part.
     */
    public BlockBodyDecl(SourceLocation location, Declaration decl) {
        super(location);
        this.decl = decl;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBody#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.decl.executeGeneric(frame);
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
