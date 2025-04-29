//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the unit literal in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class UnitLiteral extends Expr {

    // ----- Constructors -----

    /**
     * Create a unit literal node.
     *
     * @param location The location of the node in the source.
     */
    public UnitLiteral(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return LKQLUnit.INSTANCE;
    }

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.expressions.Expr#executeUnit(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public LKQLUnit executeUnit(VirtualFrame frame) {
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
