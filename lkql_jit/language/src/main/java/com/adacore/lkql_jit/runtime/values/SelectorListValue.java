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

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.nodes.dispatchers.SelectorDispatcher;
import com.adacore.lkql_jit.nodes.dispatchers.SelectorDispatcherNodeGen;
import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.runtime.values.interfaces.LazyCollection;
import com.oracle.truffle.api.CompilerDirectives;
import com.adacore.libadalang.Libadalang;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;


/**
 * This class represents a list returned by a selector call, this is a lazy list
 *
 * @author Hugo GUERRIER
 */
public final class SelectorListValue extends LazyCollection {

    // ----- Attributes -----

    /** The cache in a set to perform recursion guard */
    private final HashSet<DepthNode> hashCache;

    /** The root node of the selector */
    private final SelectorRootNode rootNode;

    /** The dispatcher for the selector root node */
    private final SelectorDispatcher dispatcher;

    /** The list of the node to recurs on */
    private final List<DepthNode> recursList;

    /** The maximal depth for the return */
    private final int maxDepth;

    /** The minimal depth for the return */
    private final int minDepth;

    /** The precise depth to get from the selector */
    private final int depth;

    // ----- Constructors -----

    /**
     * Create a new selector list
     *
     * @param rootNode The selector root node
     * @param adaNode The ada node
     * @param maxDepth The maximum depth of the returned nodes
     * @param minDepth The minimum depth of the returned nodes
     * @param depth The precise required depth of the returned nodes
     */
    @CompilerDirectives.TruffleBoundary
    public SelectorListValue(
            SelectorRootNode rootNode,
            Libadalang.AdaNode adaNode,
            int maxDepth,
            int minDepth,
            int depth
    ) {
        super(0);
        this.hashCache = new HashSet<>();
        this.rootNode = rootNode;
        this.dispatcher = SelectorDispatcherNodeGen.create();
        this.recursList = new LinkedList<>();
        this.recursList.add(new DepthNode(0, adaNode));
        this.maxDepth = maxDepth;
        this.minDepth = minDepth;
        this.depth = depth;
    }

    // ----- Class methods -----

    /** @see com.adacore.lkql_jit.runtime.values.interfaces.LazyCollection#initCache(int) */
    @Override
    @CompilerDirectives.TruffleBoundary
    protected void initCache(int index) {
        while(!this.recursList.isEmpty() && (this.cache.size() - 1 < index || index == -1)) {
            // Get the first recurse item and execute the selector on it
            DepthNode nextNode = this.recursList.remove(0);
            SelectorRootNode.SelectorCallResult result = this.dispatcher.executeDispatch(this.rootNode, nextNode);

            // If the result is a selector call result, do the needed operations
            if(!LKQLTypeSystemGen.isNullish(result.result())) {
                switch (result.mode()) {
                    case REC -> this.addRecursAndResult(result.result());
                    case DEFAULT -> this.addResult(result.result());
                    case SKIP -> this.addRecurs(result.result());
                }
            }
        }
    }

    /**
     * Add the object to the result cache of the selector list
     *
     * @param toAdd The object to add
     */
    private void addResult(Object toAdd) {
        // If the object is just a node
        if(toAdd instanceof DepthNode node) {
            if(this.isRecursionGuarded(node)) {
                this.addNodeResult(node);
            }
        }

        // If the object is an array of node
        else if(toAdd instanceof DepthNode[] nodes) {
            for (DepthNode node : nodes) {
                if(this.isRecursionGuarded(node)) {
                    this.addNodeResult(node);
                }
            }
        }
    }

    /**
     * Add the object to the recursing list of the selector list
     *
     * @param toAdd The object to add
     */
    private void addRecurs(Object toAdd) {
        // If the object is just a node
        if(toAdd instanceof DepthNode node) {
            if(this.isRecursionGuarded(node)) {
                this.recursList.add(node);
            }
        }

        // If the object is an array of node
        else if(toAdd instanceof DepthNode[] nodes) {
            for(DepthNode node : nodes) {
                if(this.isRecursionGuarded(node)) {
                    this.recursList.add(node);
                }
            }
        }
    }

    /**
     * Add the object to the result and the recursing list
     *
     * @param toAdd The object to add
     */
    private void addRecursAndResult(Object toAdd) {
        // If the object is just a node
        if(toAdd instanceof DepthNode node) {
            if(this.isRecursionGuarded(node)) {
                this.addNodeResult(node);
                this.recursList.add(node);
            }
        }

        // If the object is an array of node
        else if(toAdd instanceof DepthNode[] nodes) {
            for(DepthNode node : nodes) {
                if(this.isRecursionGuarded(node)) {
                    this.addNodeResult(node);
                    this.recursList.add(node);
                }
            }
        }
    }

    /**
     * Add a node in the result and hashed cache with all verifications
     *
     * @param node The node to add in the cache
     */
    private void addNodeResult(DepthNode node) {
        // If there is no defined depth
        if(this.depth < 0) {
            if(
                    (this.maxDepth < 0 || node.getDepth() <= this.maxDepth) &&
                    (this.minDepth < 0 || node.getDepth() >= this.minDepth)
            ) {
                this.cache.add(node);
                this.hashCache.add(node);
            }
        }

        // Else, only get the wanted nodes
        else {
            if(node.getDepth() == this.depth) {
                this.cache.add(node);
                this.hashCache.add(node);
            }
        }
    }

    /**
     * Get if a node is recursion guarded
     *
     * @param node The node to verify
     * @return True if this node is recursion guarded, false else
     */
    private boolean isRecursionGuarded(DepthNode node) {
        return !this.hashCache.contains(node);
    }

}
