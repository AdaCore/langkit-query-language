/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.nodes.arguments;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents a list of arguments in a function call.
 *
 * @author Hugo GUERRIER
 */
public final class ArgList extends LKQLNode {

    // ----- Children -----

    /**
     * The arguments in the list.
     */
    @Children
    private final Arg[] args;

    // ----- Constructors -----

    /**
     * Create a new argument list with its arguments.
     *
     * @param location The location of the argument list in the source.
     * @param args     The arguments in the list.
     */
    public ArgList(
        SourceLocation location,
        Arg[] args
    ) {
        super(location);
        this.args = args;
    }

    // ----- Getters -----

    public Arg[] getArgs() {
        return this.args;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
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
     * @param frame      The frame to execute the arguments in.
     * @param paramNames The ordered names of the parameters.
     * @return The array with the argument values.
     */
    public Object[] executeArgList(VirtualFrame frame, String[] paramNames) {
        return this.executeArgList(frame, paramNames, 0);
    }

    /**
     * Execute the argument list and get the array of evaluated argument.
     *
     * @param frame      The frame to execute the arguments in.
     * @param paramNames The names of the parameters.
     * @param offset     The offset of the first argument.
     * @return The array with the ordered and offset argument values.
     */
    public Object[] executeArgList(VirtualFrame frame, String[] paramNames, int offset) {
        // Verify the arity
        if (paramNames.length < this.args.length) {
            throw LKQLRuntimeException.wrongArity(
                paramNames.length,
                args.length,
                this
            );
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

            if (index > -1) {
                res[index] = arg.executeGeneric(frame);
            } else {
                throw LKQLRuntimeException.unknownArgument(arg.getArgName().getName(), arg.getArgName());
            }
        }

        // Return the result array
        return res;
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
