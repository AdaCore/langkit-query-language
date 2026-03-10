//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.lists;

import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.values.LKQLTuple;
import com.adacore.lkql_jit.values.interfaces.Iterable;
import com.adacore.lkql_jit.values.interfaces.Iterator;

/** This class represents the result of a mapping operation on a lazy list. */
public class LKQLEnumerateResult extends BaseLKQLLazyList {

    // ----- Attributes -----

    /** Collection that is mapped. */
    private final Iterator iterator;

    /** Next index to return. */
    private long index;

    // ----- Constructors -----

    public LKQLEnumerateResult(Iterable generator) {
        super(new ListStorage<>(16));
        this.iterator = generator.iterator();
        this.index = 1;
    }

    // ----- Lazy list required methods -----

    @Override
    protected void initCacheTo(long n) {
        while (this.iterator.hasNext() && (n >= this.cache.size() || n < 0)) {
            this.cache.append(new LKQLTuple(new Object[] { index++, iterator.next() }));
        }
    }
}
