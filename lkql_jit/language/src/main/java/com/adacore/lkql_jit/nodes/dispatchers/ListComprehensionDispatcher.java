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

package com.adacore.lkql_jit.nodes.dispatchers;

import com.adacore.lkql_jit.nodes.root_nodes.ListComprehensionRootNode;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;


/**
 * This node is a dispatcher for the list comprehension root node execution
 *
 * @author Hugo GUERRIER
 */
public abstract class ListComprehensionDispatcher extends Node {

    // ----- Execution methods -----

    /**
     * Execute the list comprehension root node with the argument for iteration
     *
     * @param rootNode  The root node to execute
     * @param arguments The arguments for the execution
     * @return The result of the list comprehension root node
     */
    public abstract Object executeDispatch(ListComprehensionRootNode rootNode, Object[] arguments);

    /**
     * Execute the list comprehension root node with the direct strategy
     *
     * @param rootNode       The root node to execute
     * @param arguments      The arguments for the execution
     * @param directCallNode The direct call node
     * @return The result of the execution
     */
    @Specialization(guards = "rootNode.getRealCallTarget() == directCallNode.getCallTarget()")
    protected static Object executeCached(
        ListComprehensionRootNode rootNode,
        Object[] arguments,
        @Cached("create(rootNode.getRealCallTarget())") DirectCallNode directCallNode
    ) {
        return directCallNode.call(arguments);
    }

    /**
     * Execute the list comprehension root node with the indirect strategy
     *
     * @param rootNode         The root node to execute
     * @param arguments        The arguments for the execution
     * @param indirectCallNode The indirect call node
     * @return The result of the execution
     */
    @Specialization(replaces = "executeCached")
    protected static Object executeUncached(
        ListComprehensionRootNode rootNode,
        Object[] arguments,
        @Cached IndirectCallNode indirectCallNode
    ) {
        return indirectCallNode.call(rootNode.getRealCallTarget(), arguments);
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return "ListComprehensionDispatcher";
    }

}
