//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.lists;

import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.LKQLDepthValue;
import com.adacore.lkql_jit.runtime.values.LKQLRecValue;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;
import java.util.ArrayDeque;
import java.util.HashSet;

/** This class represents the list returned by a selector call in the LKQL language. */
public class LKQLSelectorList extends LKQLLazyList {

    // ----- Attributes -----

    /** Direct call node used to execute the selector logic. */
    private final DirectCallNode callNode;

    /** Pre-allocated array for arguments used to call the selector body. */
    private final Object[] arguments;

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

    /** Whether to check if there is cycles in the selector list. */
    private final boolean checkCycles;

    // ----- Constructors -----

    /**
     * Create a new selector list.
     */
    @CompilerDirectives.TruffleBoundary
    public LKQLSelectorList(
        RootNode rootNode,
        Closure closure,
        Object value,
        int maxDepth,
        int minDepth,
        int depth,
        boolean checkCycles
    ) {
        this.arguments = new Object[3];
        this.arguments[0] = closure.getContent();
        this.callNode = DirectCallNode.create(rootNode.getCallTarget());
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
            arguments[1] = nextNode.value;
            arguments[2] = (long) nextNode.depth;
            LKQLRecValue result = (LKQLRecValue) this.callNode.call(arguments);

            // Add the call result to the result and recurse list
            addToRecurse(result.recurseVal, result.depth);
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
    private void addToRecurse(Object[] toAdd, int depth) {
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
    public Object get(long i) throws IndexOutOfBoundsException {
        this.computeItemAt(i);
        return this.cache.get((int) i);
    }
}
