//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.dispatchers;

import com.adacore.lkql_jit.built_ins.values.LKQLDepthValue;
import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.runtime.Cell;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;

/**
 * This node the dispatcher for the selector root nodes execution.
 *
 * @author Hugo GUERRIER
 */
public abstract class SelectorDispatcher extends Node {

    /** Function to execute the selector root node and get the result. */
    public abstract SelectorRootNode.SelectorCallResult executeDispatch(
            SelectorRootNode rootNode, Cell[] closure, LKQLDepthValue node);

    /**
     * Execute the selector root node with the direct path.
     *
     * @param rootNode The selector root node to execute.
     * @param node The node to execute the selector on.
     * @param directCallNode The direct call node.
     * @return The result of the selector call.
     */
    @Specialization(guards = "rootNode.getRealCallTarget() == directCallNode.getCallTarget()")
    protected static SelectorRootNode.SelectorCallResult executeCached(
            SelectorRootNode rootNode,
            Cell[] closure,
            LKQLDepthValue node,
            @Cached("create(rootNode.getRealCallTarget())") DirectCallNode directCallNode) {
        return (SelectorRootNode.SelectorCallResult) directCallNode.call(closure, node);
    }

    /**
     * Execute the selector root node in an indirect way.
     *
     * @param rootNode The selector root node to execute.
     * @param node The node to execute the selector on.
     * @param indirectCallNode The indirect call node.
     * @return The result of the selector call.
     */
    @Specialization(replaces = "executeCached")
    protected static SelectorRootNode.SelectorCallResult executeUncached(
            SelectorRootNode rootNode,
            Cell[] closure,
            LKQLDepthValue node,
            @Cached IndirectCallNode indirectCallNode) {
        return (SelectorRootNode.SelectorCallResult)
                indirectCallNode.call(rootNode.getRealCallTarget(), closure, node);
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return "SelectorDispatcher";
    }
}
