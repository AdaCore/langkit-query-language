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

import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.runtime.values.DepthNode;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;


/**
 * This node the dispatcher for the selector root nodes execution
 *
 * @author Hugo GUERRIER
 */
public abstract class SelectorDispatcher extends Node {

    /**
     * Function to execute the selector root node and get the result
     */
    public abstract SelectorRootNode.SelectorCallResult executeDispatch(SelectorRootNode rootNode, DepthNode node);

    /**
     * Execute the selector root node with the direct path
     *
     * @param rootNode       The selector root node to execute
     * @param node           The node to execute the selector on
     * @param directCallNode The direct call node
     * @return The result of the selector call
     */
    @Specialization(guards = "rootNode.getRealCallTarget() == directCallNode.getCallTarget()")
    protected static SelectorRootNode.SelectorCallResult executeCached(
        SelectorRootNode rootNode,
        DepthNode node,
        @Cached("create(rootNode.getRealCallTarget())") DirectCallNode directCallNode
    ) {
        return (SelectorRootNode.SelectorCallResult) directCallNode.call(node);
    }

    /**
     * Execute the selector root node in an indirect way
     *
     * @param rootNode         The selector root node to execute
     * @param node             The node to execute the selector on
     * @param indirectCallNode The indirect call node
     * @return The result of the selector call
     */
    @Specialization(replaces = "executeCached")
    protected static SelectorRootNode.SelectorCallResult executeUncached(
        SelectorRootNode rootNode,
        DepthNode node,
        @Cached IndirectCallNode indirectCallNode
    ) {
        return (SelectorRootNode.SelectorCallResult) indirectCallNode.call(rootNode.getRealCallTarget(), node);
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return "SelectorDispatcher";
    }

}
