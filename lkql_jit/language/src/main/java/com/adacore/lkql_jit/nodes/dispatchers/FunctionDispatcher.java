/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.nodes.dispatchers;

import com.adacore.lkql_jit.runtime.values.FunctionValue;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;

/**
 * This node is a dispatcher for the function execution in the LKQL JIT.
 *
 * @author Hugo GUERRIER
 */
public abstract class FunctionDispatcher extends Node {

    // ----- Execution methods -----

    /** Method to enter the dispatch of the node. */
    public abstract Object executeDispatch(FunctionValue function, Object[] arguments);

    /**
     * Execute the function via a cached direct node.
     *
     * @param function The function value to execute.
     * @param arguments The calling arguments.
     * @param directCallNode The direct call node.
     * @return The result of the function execution.
     */
    @Specialization(guards = "function.getCallTarget() == directCallNode.getCallTarget()")
    protected static Object dispatchDirect(
            @SuppressWarnings("unused") FunctionValue function,
            Object[] arguments,
            @Cached("create(function.getCallTarget())") DirectCallNode directCallNode) {
        return directCallNode.call(arguments);
    }

    /**
     * Execute the function with an indirect call.
     *
     * @param function The function value to execute.
     * @param arguments The function call arguments.
     * @param indirectCallNode The indirect call node.
     * @return The result of the function execution.
     */
    @Specialization(replaces = "dispatchDirect")
    protected static Object dispatchIndirect(
            FunctionValue function, Object[] arguments, @Cached IndirectCallNode indirectCallNode) {
        return indirectCallNode.call(function.getCallTarget(), arguments);
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return "FunctionDispatcher";
    }
}
