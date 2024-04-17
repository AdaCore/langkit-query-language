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
 * This node represents the base of unary operations in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@NodeChild(value = "arg", type = Expr.class)
public abstract class UnOp extends Expr {

    // ----- Attributes -----

    /** The location of the argument node. */
    protected final DummyLocation argLocation;

    // ----- Constructors -----

    /**
     * Create a unary operation node.
     *
     * @param location The location of the node in the source.
     * @param argLocation The location of the argument node.
     */
    protected UnOp(SourceLocation location, DummyLocation argLocation) {
        super(location);
        this.argLocation = argLocation;
    }
}
