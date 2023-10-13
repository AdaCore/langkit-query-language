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

package com.adacore.lkql_jit.built_ins.values.lists;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.values.LKQLDepthNode;
import com.adacore.lkql_jit.nodes.dispatchers.SelectorDispatcher;
import com.adacore.lkql_jit.nodes.dispatchers.SelectorDispatcherNodeGen;
import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

/** This class represents the list returned by a selector call in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public class LKQLSelectorList extends LKQLLazyList {

    // ----- Attributes -----

    /** The root node of the selector. */
    private final SelectorRootNode rootNode;

    /** The closure for the root node execution. */
    private final Closure closure;

    /** The dispatcher for the selector root node. */
    private final SelectorDispatcher dispatcher;

    /** The cache of already explored nodes. */
    private final HashSet<LKQLDepthNode> alreadyVisited;

    /** The list of the node to recurs on. */
    private final List<LKQLDepthNode> recursList;

    /** The maximal depth for the return. */
    private final int maxDepth;

    /** The minimal depth for the return. */
    private final int minDepth;

    /** The precise depth to get from the selector. */
    private final int depth;

    // ----- Constructors -----

    /**
     * Create a new selector list.
     *
     * @param rootNode The selector root node.
     * @param closure The closure for the root node execution.
     * @param adaNode The ada node.
     * @param maxDepth The maximum depth of the returned nodes.
     * @param minDepth The minimum depth of the returned nodes.
     * @param depth The precise required depth of the returned nodes.
     */
    @CompilerDirectives.TruffleBoundary
    public LKQLSelectorList(
            final SelectorRootNode rootNode,
            final Closure closure,
            final Libadalang.AdaNode adaNode,
            final int maxDepth,
            final int minDepth,
            final int depth) {
        this.rootNode = rootNode;
        this.closure = closure;
        this.dispatcher = SelectorDispatcherNodeGen.create();
        this.alreadyVisited = new HashSet<>();
        this.recursList = new LinkedList<>();
        this.maxDepth = maxDepth;
        this.minDepth = minDepth;
        this.depth = depth;
        this.recursList.add(new LKQLDepthNode(0, adaNode));
    }

    // ----- Lazy list required methods -----

    @Override
    public void initCache(int n) {
        while (!(this.recursList.size() == 0) && (this.cache.size() - 1 < n || n == -1)) {
            // Get the first recurse item and execute the selector on it
            LKQLDepthNode nextNode = this.recursList.remove(0);
            SelectorRootNode.SelectorCallResult result =
                    this.dispatcher.executeDispatch(
                            this.rootNode, this.closure.getContent(), nextNode);

            // If the result is a selector call result, do the needed operations
            if (!LKQLTypeSystemGen.isNullish(result.result())) {
                switch (result.mode()) {
                    case REC -> this.addRecursAndResult(result.result());
                    case DEFAULT -> this.addResult(result.result());
                    case SKIP -> this.addRecurs(result.result());
                }
            }
        }
    }

    /** Add the object to the result cache of the selector list. */
    @CompilerDirectives.TruffleBoundary
    private void addResult(Object toAdd) {
        // If the object is just a node
        if (toAdd instanceof LKQLDepthNode node) {
            if (!this.alreadyVisited.contains(node)) {
                this.addNodeResult(node);
            }
        }

        // If the object is an array of node
        else if (toAdd instanceof LKQLDepthNode[] nodes) {
            for (LKQLDepthNode node : nodes) {
                if (!this.alreadyVisited.contains(node)) {
                    this.addNodeResult(node);
                }
            }
        }
    }

    /** Add the object to the recursing list of the selector list. */
    @CompilerDirectives.TruffleBoundary
    private void addRecurs(Object toAdd) {
        // If the object is just a node
        if (toAdd instanceof LKQLDepthNode node) {
            if (!this.alreadyVisited.contains(node)) {
                this.recursList.add(node);
            }
        }

        // If the object is an array of node
        else if (toAdd instanceof LKQLDepthNode[] nodes) {
            for (LKQLDepthNode node : nodes) {
                if (!this.alreadyVisited.contains(node)) {
                    this.recursList.add(node);
                }
            }
        }
    }

    /** Add the object to the result and the recursing list. */
    @CompilerDirectives.TruffleBoundary
    private void addRecursAndResult(Object toAdd) {
        // If the object is just a node
        if (toAdd instanceof LKQLDepthNode node) {
            if (!this.alreadyVisited.contains(node)) {
                this.addNodeResult(node);
                this.recursList.add(node);
            }
        }

        // If the object is an array of node
        else if (toAdd instanceof LKQLDepthNode[] nodes) {
            for (LKQLDepthNode node : nodes) {
                if (!this.alreadyVisited.contains(node)) {
                    this.addNodeResult(node);
                    this.recursList.add(node);
                }
            }
        }
    }

    /** Add a node in the result and hashed cache with all verifications. */
    @CompilerDirectives.TruffleBoundary
    private void addNodeResult(LKQLDepthNode node) {
        // If there is no defined depth
        if (this.depth < 0) {
            if ((this.maxDepth < 0 || node.getDepth() <= this.maxDepth)
                    && (this.minDepth < 0 || node.getDepth() >= this.minDepth)) {
                this.cache.add(node);
                this.alreadyVisited.add(node);
            }
        }

        // Else, only get the wanted nodes
        else {
            if (node.getDepth() == this.depth) {
                this.cache.add(node);
                this.alreadyVisited.add(node);
            }
        }
    }

    // ----- Value methods -----

    /** Return the identity hash code for the given LKQL selector list. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public static int identityHashCode(LKQLSelectorList receiver) {
        return System.identityHashCode(receiver);
    }
}
