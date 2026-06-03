//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.streams;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.LKQLDepthValue;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import java.util.ArrayDeque;
import java.util.HashSet;

/** This class represents the stream returned by a selector call in the LKQL language. */
public class LKQLSelectorList extends BaseCachedStream {

    // ----- Attributes -----

    /** Root for the selector execution. */
    private final FunctionRootNode rootNode;

    /** Call target representing the selector execution. */
    private final CallTarget callTarget;

    /** Pre-allocated array for arguments used to call the selector body. */
    private final Object[] arguments;

    /** The cache of already explored nodes. */
    private final HashSet<LKQLDepthValue> alreadyVisited;

    /** The list of values to visit. */
    private final ArrayDeque<LKQLDepthValue> toVisitList;

    /** The maximal depth for the return. */
    private final long maxDepth;

    /** The minimal depth for the return. */
    private final long minDepth;

    /** The precise depth to get from the selector. */
    private final long exactDepth;

    // ----- Constructors -----

    /**
     * Create a new selector list.
     */
    @CompilerDirectives.TruffleBoundary
    public LKQLSelectorList(
        FunctionRootNode rootNode,
        Closure closure,
        Object value,
        long depth,
        long maxDepth,
        long minDepth
    ) {
        super(new ListStorage<>(16));
        this.arguments = new Object[2];
        this.arguments[0] = closure;
        this.rootNode = rootNode;
        this.callTarget = rootNode.getCallTarget();
        this.toVisitList = new ArrayDeque<>();
        this.maxDepth = maxDepth;
        this.minDepth = minDepth;
        this.exactDepth = depth;
        this.toVisitList.add(new LKQLDepthValue(0, value));
        // We only check cycles on memoized selectors for now
        if (rootNode.isMemoized()) {
            this.alreadyVisited = new HashSet<>();
        } else {
            this.alreadyVisited = null;
        }
    }

    // ----- Getters -----

    /** Should the selector list check for cycles. */
    private boolean shouldCheckCycles() {
        return this.alreadyVisited != null;
    }

    // ----- Instance methods -----

    /** Stub for the compiler. Do not use. */
    protected Object computeNext() {
        return null;
    }

    @Override
    public Object get(long n) {
        while (!(this.toVisitList.size() == 0) && (this.cache.size() - 1 < n || n < 0)) {
            // Get the first recurse item and execute the selector on it
            LKQLDepthValue input = this.toVisitList.poll();
            arguments[1] = input.value;
            final var result = callTarget.call(arguments);
            final int resultDepth = input.depth + 1;

            if (LKQLTypeSystemGen.isLKQLRecValue(result)) {
                final var res = LKQLTypeSystemGen.asLKQLRecValue(result);
                // Add the call result to the result and recurse list

                if (shouldCheckCycles()) {
                    addToRecurseAndCheckCycles(res.recurseVal, resultDepth);
                } else {
                    addToRecurse(res.recurseVal, resultDepth);
                }

                if (isValidDepth(resultDepth)) {
                    addToResult(res.resultVal);
                }
            } else if (!LKQLTypeSystemGen.isNullish(result)) {
                throw LKQLRuntimeError.wrongType(
                    LKQLTypesHelper.LKQL_REC_VALUE,
                    LKQLTypesHelper.fromJava(result),
                    rootNode.getBody()
                );
            }
        }
        return this.cache.get((int) n);
    }

    /** Add the object to the result cache of the selector list. */
    @CompilerDirectives.TruffleBoundary
    private void addToResult(Object[] toAdd) {
        for (var val : toAdd) {
            cache.append(val);
        }
    }

    /** Add the object to the recursing list of the selector list. */
    @CompilerDirectives.TruffleBoundary
    private void addToRecurse(Object[] toAdd, int depth) {
        for (var val : toAdd) {
            var depthVal = new LKQLDepthValue(depth, val);
            this.toVisitList.add(depthVal);
        }
    }

    /** Add the object to the recursing list of the selector list if there is no cycle. */
    @CompilerDirectives.TruffleBoundary
    private void addToRecurseAndCheckCycles(Object[] toAdd, int depth) {
        for (var val : toAdd) {
            var depthVal = new LKQLDepthValue(depth, val);
            if (!this.alreadyVisited.contains(depthVal)) {
                this.toVisitList.add(depthVal);
                this.alreadyVisited.add(depthVal);
            }
        }
    }

    /** Tests if depth is in the valid range. */
    private boolean isValidDepth(int depth) {
        // If there is no defined depth
        if (this.exactDepth < 0) {
            return (
                (this.maxDepth < 0 || depth <= this.maxDepth) &&
                (this.minDepth < 0 || depth >= this.minDepth)
            );
        }
        // Else, only get the wanted nodes
        else {
            return depth == this.exactDepth;
        }
    }
}
