//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a null literal in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class NullLiteral extends Expr {

    // ----- Constructors -----

    /**
     * Create a null literal node.
     *
     * @param location The location of the node in the source.
     */
    public NullLiteral(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return LKQLNull.INSTANCE;
    }

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.expressions.Expr#executeNode(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Libadalang.AdaNode executeNode(VirtualFrame frame) {
        return LKQLNull.INSTANCE;
    }

    // ----- Override methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
