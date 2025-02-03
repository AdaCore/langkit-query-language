//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.arguments;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
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

    /**
     * Execute the argument list according to its order.
     *
     * @param frame The frame to execute the arguments in.
     * @return The value of the arguments.
     */
    public Object[] executeArgList(VirtualFrame frame) {
        Object[] res = new Object[this.args.length];
        for (int i = 0; i < res.length; i++) {
            res[i] = this.args[i].executeGeneric(frame);
        }
        return res;
    }

    /**
     * Execute the argument list and order the results according to the parameter names.
     *
     * @param frame The frame to execute the arguments in.
     * @param paramNames The ordered names of the parameters.
     * @return The array with the argument values.
     */
    public Object[] executeArgList(VirtualFrame frame, String[] paramNames) {
        return this.executeArgList(frame, paramNames, 0);
    }

    /**
     * Execute the argument list and get the array of evaluated argument.
     *
     * @param frame The frame to execute the arguments in.
     * @param paramNames The names of the parameters.
     * @param offset The offset of the first argument.
     * @return The array with the ordered and offset argument values.
     */
    public Object[] executeArgList(VirtualFrame frame, String[] paramNames, int offset) {
        // Verify the arity
        if (paramNames.length < this.args.length) {
            throw LKQLRuntimeException.wrongArity(paramNames.length, args.length, this);
        }

        // Prepare the result
        Object[] res = new Object[paramNames.length];

        // Fill the result
        for (int i = 0; i < this.args.length; i++) {
            // Prepare the turn
            Arg arg = this.args[i];
            int index = i + offset;

            // If the current argument is a named arg
            if (arg instanceof NamedArg namedArg) {
                index = ArrayUtils.indexOf(paramNames, namedArg.getArgName().getName());
            }

            //  There is no corresponding parameter
            if (index > -1) {
                res[index] = arg.executeGeneric(frame);
            } else {
                throw LKQLRuntimeException.unknownArgument(
                    arg.getArgName().getName(),
                    arg.getArgName()
                );
            }
        }

        // Return the result array
        return res;
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
