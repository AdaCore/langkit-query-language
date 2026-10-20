//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.values.LKQLUnit;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents the unit literal. */
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

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return LKQLUnit.INSTANCE;
    }

    @Override
    public LKQLUnit executeUnit(VirtualFrame frame) {
        return LKQLUnit.INSTANCE;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
