//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes;

import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This class represents an textual identifier. Note that it extends Expr even though it is not an
 * executable node. This node is used to associate a location to a symbol.
 */
public final class Identifier extends Expr {

    // ----- Attributes -----

    /** The name of the identifier. */
    private final String name;

    // ----- Constructors -----

    /** Create a new identifier. */
    public Identifier(SourceSection location, String name) {
        super(location);
        this.name = name;
    }

    // ----- Getters -----

    public String getName() {
        return this.name;
    }

    // ----- Override methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLEngineException.shouldNotReachHere();
    }

    @Override
    public String toString() {
        return this.name;
    }

    @Override
    public String toString(int indentLevel) {
        return this.name;
    }
}
