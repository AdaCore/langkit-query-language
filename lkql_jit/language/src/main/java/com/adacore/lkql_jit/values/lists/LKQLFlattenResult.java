//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.lists;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.interfaces.Iterable;
import com.adacore.lkql_jit.values.interfaces.Iterator;

public final class LKQLFlattenResult extends BaseLKQLLazyList {

    /** Collection that is flatten. */
    private final Iterator generatorIterator;

    /** Iterator used to go though inner collections. */
    private Iterator innerIterator;

    // ----- Constructors -----

    public LKQLFlattenResult(Iterable generator) {
        super(new ListStorage<>(32));
        this.generatorIterator = generator.iterator();
    }

    // ----- Lazy list required methods -----

    @Override
    protected void initCacheTo(long n) {
        if (n >= this.cache.size() || n < 0) {
            Object next;
            do {
                next = this.nextElem();
                if (next != null) {
                    this.cache.append(next);
                }
            } while (next != null && (n >= this.cache.size() || n < 0));
        }
    }

    // ----- Instance methods -----

    /**
     * Util function to get the next element in the current inner collection.
     * If there is no more elements in the current inner collection, this
     * function fetch the next one.
     * If there is no more inner collection, this function returns null.
     */
    private Object nextElem() {
        // Ensure the inner iterator is initialized and has a next element
        while (this.innerIterator == null || !this.innerIterator.hasNext()) {
            if (!this.generatorIterator.hasNext()) {
                return null;
            }
            var nextInner = this.generatorIterator.next();
            if (nextInner instanceof Iterable nextInnerIterable) {
                this.innerIterator = nextInnerIterable.iterator();
            } else {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_ITERABLE,
                    LKQLTypesHelper.fromJava(nextInner),
                    null
                );
            }
        }

        // Finally return the next element
        return this.innerIterator.next();
    }
}
