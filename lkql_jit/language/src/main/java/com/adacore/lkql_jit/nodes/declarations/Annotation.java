//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents an annotation associated with a declaration in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class Annotation extends LKQLNode {

    // ----- Attributes -----

    /** The name of the annotation. */
    private final String name;

    // ----- Children -----

    /** The annotation arguments. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private ArgList arguments;

    // ----- Constructors -----

    /**
     * Create a new declaration annotation node.
     *
     * @param location The location of the node in the source.
     * @param name The name of the annotation.
     * @param arguments The arguments of the annotation (can be empty or null).
     */
    public Annotation(SourceSection location, String name, ArgList arguments) {
        super(location);
        this.name = name;
        this.arguments = arguments;
    }

    // ----- Getters -----

    public String getName() {
        return name;
    }

    public ArgList getArguments() {
        return arguments;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    // ----- Override methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "name" },
                new Object[] { this.name }
            );
    }
}
