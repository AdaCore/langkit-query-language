//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.lists;

import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterator;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * This class represents either
 * - a list comprehension value
 * - a query expression
 * in the LKQL language.
 * It allows iterating on a source and
 * - pattern matching values from it
 * - checking against a boolean guard
 * - mapping to another value
 * and returns the result as a LazyList.
 */
public final class LKQLQueryComprehension extends BaseLKQLLazyList {

    // ----- Attributes -----

    private final DirectCallNode callNode;

    private final Iterator iterator;

    /**
     * argument[0] is the closure
     * argument[1] is the iterator result
     */
    private final Object[] arguments = new Object[2];

    // ----- Constructors -----

    @CompilerDirectives.TruffleBoundary
    public LKQLQueryComprehension(
        final RootNode rootNode,
        final Closure closure,
        final Iterable source
    ) {
        super(new ListStorage<>(1));
        this.callNode = DirectCallNode.create(rootNode.getCallTarget());
        this.iterator = source.iterator();
        this.arguments[0] = closure.getContent();
    }

    // ----- Lazy list required methods -----

    @Override
    protected void initCacheTo(long n) {
        while ((n < 0 || this.cache.size() <= n) && iterator.hasNext()) {
            this.arguments[1] = iterator.next();
            Object value = this.callNode.call(this.arguments);
            if (value != null) this.cache.append(value);
        }
    }
}
