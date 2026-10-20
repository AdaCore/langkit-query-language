//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.values.LKQLNull;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a null literal */
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

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return LKQLNull.INSTANCE;
    }

    @Override
    public LangkitSupport.NodeInterface executeNode(VirtualFrame frame) {
        return LKQLNull.INSTANCE;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
