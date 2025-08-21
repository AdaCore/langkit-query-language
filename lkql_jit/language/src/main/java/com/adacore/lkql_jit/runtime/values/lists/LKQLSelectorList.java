//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.lists;

import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.nodes.dispatchers.SelectorDispatcher;
import com.adacore.lkql_jit.nodes.dispatchers.SelectorDispatcherNodeGen;
import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.LKQLDepthValue;
import com.adacore.lkql_jit.runtime.values.LKQLRecValue;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.ArrayDeque;
import java.util.HashSet;

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

    /** The list of values to visit. */
    private final ArrayDeque<LKQLDepthValue> toVisitList;

    /** The maximal depth for the return. */
    private final int maxDepth;

    /** The minimal depth for the return. */
    private final int minDepth;

    /** The precise depth to get from the selector. */
    private final int exactDepth;
    private final boolean checkCycles;

    // ----- Constructors -----

    /**
     * Create a new selector list.
     */
    @CompilerDirectives.TruffleBoundary
    public LKQLSelectorList(
        SelectorRootNode rootNode,
        Closure closure,
        Object value,
        int maxDepth,
        int minDepth,
        int depth,
        boolean checkCycles
    ) {
        this.rootNode = rootNode;
        this.closure = closure;
        this.dispatcher = SelectorDispatcherNodeGen.create();
        this.toVisitList = new ArrayDeque<>();
        this.maxDepth = maxDepth;
        this.minDepth = minDepth;
        this.exactDepth = depth;
        this.toVisitList.add(new LKQLDepthValue(0, value));
        this.checkCycles = checkCycles;
        if (checkCycles) {
            this.alreadyVisited = new HashSet<>();
        } else {
            this.alreadyVisited = null;
        }
    }

    // ----- Lazy list required methods -----

    @Override
    public void computeItemAt(long n) {
        while (!(this.toVisitList.size() == 0) && (this.cache.size() - 1 < n || n == -1)) {
            // Get the first recurse item and execute the selector on it
            LKQLDepthValue nextNode = this.toVisitList.poll();
            LKQLRecValue result =
                this.dispatcher.executeDispatch(
                        this.rootNode,
                        this.closure.getContent(),
                        nextNode.value,
                        nextNode.depth
                    );

            addToRecurs(result.recurseVal, result.depth);
            addToResult(result.resultVal, result.depth);
        }
    }

    /** Add the object to the result cache of the selector list. */
    @CompilerDirectives.TruffleBoundary
    private void addToResult(Object[] toAdd, int depth) {
        for (var val : toAdd) {
            this.addResult(val, depth);
        }
    }

    /** Add the object to the recursing list of the selector list. */
    @CompilerDirectives.TruffleBoundary
    private void addToRecurs(Object[] toAdd, int depth) {
        for (var val : toAdd) {
            var depthVal = new LKQLDepthValue(depth, val);
            if (!checkCycles) {
                this.toVisitList.add(depthVal);
            } else if (!this.alreadyVisited.contains(depthVal)) {
                this.toVisitList.add(depthVal);
                this.alreadyVisited.add(depthVal);
            }
        }
    }

    /** Add a node in the result and hashed cache with all verifications. */
    private void addResult(Object value, int depth) {
        // If there is no defined depth
        if (this.exactDepth < 0) {
            if (
                (this.maxDepth < 0 || depth <= this.maxDepth) &&
                (this.minDepth < 0 || depth >= this.minDepth)
            ) {
                this.cache.append(value);
            }
        }
        // Else, only get the wanted nodes
        else {
            if (depth == this.exactDepth) {
                this.cache.append(value);
            }
        }
    }

    @Override
    public Object get(long i) throws InvalidIndexException {
        this.computeItemAt(i);
        try {
            return this.cache.get((int) i);
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
