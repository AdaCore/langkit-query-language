//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node is the base of all binary operations in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@NodeChild(value = "left", type = Expr.class)
@NodeChild(value = "right", type = Expr.class)
public abstract class BinOp extends Expr {

    /**
     * Create a binary operation node.
     *
     * @param location The location of the node in the source.
     */
    protected BinOp(SourceSection location) {
        super(location);
    }

    public abstract Expr getRight();

    public abstract Expr getLeft();
}
