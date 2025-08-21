//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.arguments;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import java.util.Optional;

/**
 * This node represents a list of arguments in a function call.
 *
 * @author Hugo GUERRIER
 */
public final class ArgList extends LKQLNode {

    // ----- Children -----

    /** The arguments in the list. */
    @Children
    private final Arg[] args;

    // ----- Constructors -----

    /**
     * Create a new argument list with its arguments.
     *
     * @param location The location of the argument list in the source.
     * @param args The arguments in the list.
     */
    public ArgList(SourceSection location, Arg[] args) {
        super(location);
        this.args = args;
    }

    // ----- Getters -----

    public Arg[] getArgs() {
        return this.args;
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

    // ----- Class methods -----

    /** Get the (eventually present) named argument node in the list with the provided name. */
    public Optional<Arg> getArgWithName(final String name) {
        for (var arg : this.args) {
            if (arg instanceof NamedArg namedArg && namedArg.argName.getName().equals(name)) {
                return Optional.of(namedArg);
            }
        }
        return Optional.empty();
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
