//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents the declaration of a parameter in a function signature in LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ParameterDeclaration extends Declaration {

    // ----- Attributes -----

    /** Parameter's name. */
    private final String name;

    /** Parameter's default value. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr defaultValue;

    // ----- Constructors -----

    /**
     * Create a new parameter declaration node.
     *
     * @param location The location of the node in the source.
     * @param name The name of the parameter.
     * @param defaultValue The default value of the parameter (can be null).
     */
    public ParameterDeclaration(
            final SourceLocation location, final String name, final Expr defaultValue) {
        super(location, null);
        this.name = name;
        this.defaultValue = defaultValue;
    }

    // ----- Getters -----

    public String getName() {
        return this.name;
    }

    public Expr getDefaultValue() {
        return this.defaultValue;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Fail because this node is not executable as a generic one
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"name"}, new Object[] {this.name});
    }
}
