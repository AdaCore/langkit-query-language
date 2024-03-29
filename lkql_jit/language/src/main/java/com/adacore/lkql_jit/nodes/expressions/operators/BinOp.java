//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.NodeChild;

/**
 * This node is the base of all binary operations in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@NodeChild(value = "left", type = Expr.class)
@NodeChild(value = "right", type = Expr.class)
public abstract class BinOp extends Expr {

    // ----- Attributes -----

    /** The location of the left node. */
    protected final DummyLocation leftLocation;

    /** The location of the right node. */
    protected final DummyLocation rightLocation;

    // ----- Constructors -----

    /**
     * Create a binary operation node.
     *
     * @param location The location of the node in the source.
     * @param leftLocation The location of the left node.
     * @param rightLocation The location of the right node.
     */
    protected BinOp(
            SourceLocation location, DummyLocation leftLocation, DummyLocation rightLocation) {
        super(location);
        this.leftLocation = leftLocation;
        this.rightLocation = rightLocation;
    }
}
