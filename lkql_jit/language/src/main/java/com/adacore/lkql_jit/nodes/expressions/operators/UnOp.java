//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents the base of all unary operations. */
@NodeChild(value = "arg", type = Expr.class)
public abstract class UnOp extends Expr {

    // ----- Constructors -----

    /**
     * Create a unary operation node.
     *
     * @param location The location of the node in the source.
     */
    protected UnOp(SourceSection location) {
        super(location);
    }

    // ----- Getters -----

    public abstract Expr getArg();
}
