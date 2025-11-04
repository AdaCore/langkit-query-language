//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.lists;

import com.adacore.lkql_jit.runtime.Closure;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;

/** This class represents a list comprehension value in the LKQL language. */
public final class LKQLListComprehension extends LKQLLazyList {

    // ----- Attributes -----

    /** Direct call node used to execute the list comprehension logic. */
    private final DirectCallNode callNode;

    /** List of arguments to pass to the list comprehension root node. */
    private final Object[][] argumentsList;

    /** Pre-allocated argument array which already contains the list comprehension closure. */
    private final Object[] arguments;

    /** Pointer to the current arguments to pass to the root node. */
    private int pointer;

    // ----- Constructors -----

    /**
     * Create a new LKQL list comprehension value with everything needed for its execution.
     *
     * @param rootNode The root node which contains the list comprehension logics.
     * @param closure The closure for the execution.
     * @param argumentsList The sequence of arguments to pass to the list comprehension root node.
     */
    @CompilerDirectives.TruffleBoundary
    public LKQLListComprehension(
        final RootNode rootNode,
        final Closure closure,
        final Object[][] argumentsList
    ) {
        this.callNode = DirectCallNode.create(rootNode.getCallTarget());
        this.argumentsList = argumentsList;
        this.arguments = this.argumentsList.length > 0
            ? new Object[this.argumentsList[0].length + 1]
            : new Object[1];
        this.arguments[0] = closure.getContent();
        this.pointer = 0;
    }

    // ----- Lazy list required methods -----

    @Override
    public void computeItemAt(long n) {
        while (this.pointer < this.argumentsList.length && (this.cache.size() - 1 < n || n < 0)) {
            final Object[] currentArguments = this.argumentsList[this.pointer++];
            System.arraycopy(currentArguments, 0, this.arguments, 1, currentArguments.length);
            Object value = this.callNode.call(this.arguments);
            if (value != null) {
                this.cache.append(value);
            }
        }
    }
}
