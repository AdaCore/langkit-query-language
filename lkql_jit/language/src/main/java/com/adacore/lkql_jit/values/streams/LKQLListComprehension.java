//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.streams;

import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.values.interfaces.Iterator;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.RootNode;

/** This class represents a list comprehension value in the LKQL language. */
public final class LKQLListComprehension extends BaseCachedStream {

    // ----- Attributes -----

    /** Call target representing the list comprehension execution logic. */
    private final CallTarget callTarget;

    /** Sources to pull from. */
    private final Iterator[] sources;

    /** Number of sources to pull from. */
    private int pullCount;

    /** Pre-allocated argument array which already contains the list comprehension closure. */
    private final Object[] arguments;

    // ----- Constructors -----

    /**
     * Create a new LKQL list comprehension value with everything needed for its execution.
     *
     * @param rootNode The root node which contains the list comprehension logics.
     * @param closure The closure for the execution.
     * @param sources The sources to pull combinations from and pass to the root node.
     */
    @CompilerDirectives.TruffleBoundary
    public LKQLListComprehension(
        final RootNode rootNode,
        final Closure closure,
        final Iterator[] sources
    ) {
        super(new ListStorage<>(sources.length > 0 ? 16 : 0));
        this.callTarget = rootNode.getCallTarget();
        this.sources = sources;
        this.pullCount = sources.length; // first pull from all sources
        this.arguments = new Object[sources.length + 1];
        this.arguments[0] = closure;
    }

    // ----- Instance methods -----

    @Override
    protected void initCacheTo(long n) {
        while (n >= this.cache.size() || n < 0) {
            if (!updateArgs()) return;
            Object value = callTarget.call(arguments);
            if (value != null) {
                this.cache.append(value);
            }
        }
    }

    // ----- Internal methods -----

    private boolean reachedEnd() {
        return pullCount > sources.length;
    }

    /**
     * Populates the arguments attribute.
     *
     * @return true only if succeeded.
     */
    private boolean updateArgs() {
        if (reachedEnd()) return false;
        for (int i = 0; i < pullCount; i++) {
            var idx = sources.length - 1 - i;
            var it = sources[idx];
            if (!it.hasNext()) {
                it.reset();
                pullCount++; // end of current source, should pull 1 more
                if (reachedEnd()) return false;
            }
            arguments[idx + 1] = it.next();
        }
        pullCount = 1; // try to pull 1 element next time
        return true;
    }
}
