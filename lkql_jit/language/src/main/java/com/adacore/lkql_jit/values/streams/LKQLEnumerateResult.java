//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.streams;

import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.values.LKQLTuple;
import com.adacore.lkql_jit.values.interfaces.Iterable;
import com.adacore.lkql_jit.values.interfaces.Iterator;

/** This class represents the result of a mapping operation on a stream. */
public class LKQLEnumerateResult extends BaseCachedStream {

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

    // ----- Instance methods -----

    protected Object computeNext() {
        if (!iterator.hasNext()) return null;
        return new LKQLTuple(new Object[] { index++, iterator.next() });
    }
}
