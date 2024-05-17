//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This class represents an identifier in the LKQL language. Note that it extends Expr even though
 * it is not an executable node. This is used to use `Identifier` as a valid location for errors in
 * runtime exceptions.
 */
public final class Identifier extends Expr {

    // ----- Attributes -----

    /** The name of the identifier. */
    private final String name;

    // ----- Constructors -----

    /**
     * Create a new identifier with the parameters.
     *
     * @param location The location of the identifier in the source.
     * @param name The name of the identifier.
     */
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
        throw LKQLRuntimeException.shouldNotExecute(this);
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
