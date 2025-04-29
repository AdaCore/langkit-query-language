//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.dispatchers;

import com.adacore.lkql_jit.nodes.root_nodes.ListComprehensionRootNode;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;

/**
 * This node is a dispatcher for the list comprehension root node execution.
 *
 * @author Hugo GUERRIER
 */
public abstract class ListComprehensionDispatcher extends Node {

    // ----- Execution methods -----

    /**
     * Execute the list comprehension root node with the argument for iteration.
     *
     * @param rootNode The root node to execute.
     * @param arguments The arguments for the execution.
     * @return The result of the list comprehension root node.
     */
    public abstract Object executeDispatch(ListComprehensionRootNode rootNode, Object[] arguments);

    /**
     * Execute the list comprehension root node with the direct strategy.
     *
     * @param rootNode The root node to execute.
     * @param arguments The arguments for the execution.
     * @param directCallNode The direct call node.
     * @return The result of the execution.
     */
    @Specialization(guards = "rootNode.getRealCallTarget() == directCallNode.getCallTarget()")
    protected static Object executeCached(
        @SuppressWarnings("unused") ListComprehensionRootNode rootNode,
        Object[] arguments,
        @Cached("create(rootNode.getRealCallTarget())") DirectCallNode directCallNode
    ) {
        return directCallNode.call(arguments);
    }

    /**
     * Execute the list comprehension root node with the indirect strategy.
     *
     * @param rootNode The root node to execute.
     * @param arguments The arguments for the execution.
     * @param indirectCallNode The indirect call node.
     * @return The result of the execution.
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
