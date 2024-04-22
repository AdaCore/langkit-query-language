//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.values.lists;

import com.adacore.lkql_jit.built_ins.values.LKQLDepthValue;
import com.adacore.lkql_jit.built_ins.values.LKQLRecValue;
import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
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
    private final HashSet<LKQLDepthValue> alreadyVisited;

    /** The list of the node to recurs on. */
    private final List<LKQLDepthValue> recursList;

    /** The maximal depth for the return. */
    private final int maxDepth;

    /** The minimal depth for the return. */
    private final int minDepth;

    /** The precise depth to get from the selector. */
    private final int exactDepth;

    // ----- Constructors -----

    /**
     * Create a new selector list.
     *
     * @param rootNode The selector root node.
     * @param closure The closure for the root node execution.
     * @param maxDepth The maximum depth of the returned nodes.
     * @param minDepth The minimum depth of the returned nodes.
     * @param depth The precise required depth of the returned nodes.
     */
    @CompilerDirectives.TruffleBoundary
    public LKQLSelectorList(
            final SelectorRootNode rootNode,
            final Closure closure,
            final Object value,
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
        this.exactDepth = depth;
        this.recursList.add(new LKQLDepthValue(0, value));
    }

    // ----- Lazy list required methods -----

    @Override
    public void computeItemAt(long n) {
        while (!(this.recursList.size() == 0) && (this.cache.size() - 1 < n || n == -1)) {
            // Get the first recurse item and execute the selector on it
            LKQLDepthValue nextNode = this.recursList.remove(0);
            LKQLRecValue result =
                    this.dispatcher.executeDispatch(
                            this.rootNode, this.closure.getContent(), nextNode);

            addToRecurs(result.recurseVal, result.depth);
            addToResult(result.resultVal, result.depth);
        }
    }

    /** Add the object to the result cache of the selector list. */
    @CompilerDirectives.TruffleBoundary
    private void addToResult(Object[] toAdd, int depth) {
        for (var val : toAdd) {
            var depthVal = new LKQLDepthValue(depth, val);
            this.addResult(depthVal);
        }
    }

    /** Add the object to the recursing list of the selector list. */
    @CompilerDirectives.TruffleBoundary
    private void addToRecurs(Object[] toAdd, int depth) {
        for (var val : toAdd) {
            var depthVal = new LKQLDepthValue(depth, val);
            if (!this.alreadyVisited.contains(depthVal)) {
                this.recursList.add(depthVal);
                this.alreadyVisited.add(depthVal);
            }
        }
    }

    /** Add a node in the result and hashed cache with all verifications. */
    @CompilerDirectives.TruffleBoundary
    private void addResult(LKQLDepthValue value) {
        // If there is no defined depth
        if (this.exactDepth < 0) {
            if ((this.maxDepth < 0 || value.depth <= this.maxDepth)
                    && (this.minDepth < 0 || value.depth >= this.minDepth)) {
                this.cache.add(value);
            }
        }

        // Else, only get the wanted nodes
        else {
            if (value.depth == this.exactDepth) {
                this.cache.add(value);
            }
        }
    }

    @Override
    public Object get(long i) throws InvalidIndexException {
        this.computeItemAt(i);
        try {
            var cache = this.cache.get((int) i);
            return ((LKQLDepthValue) cache).value;
        } catch (IndexOutOfBoundsException e) {
            throw new InvalidIndexException();
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
