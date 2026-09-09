//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.streams;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.values.LKQLObject;
import com.adacore.lkql_jit.values.interfaces.Iterable;
import com.adacore.lkql_jit.values.interfaces.Iterator;

/** This class represents the result of adding indexes to a stream. */
public class LKQLWithIndexResult extends BaseCachedStream {

    // ----- Attributes -----

    /** Collection that is mapped. */
    private final Iterator iterator;

    /** Next index to return. */
    private long index;

    // ----- Constructors -----

    public LKQLWithIndexResult(Iterable generator) {
        super(new ListStorage<>(16));
        this.iterator = generator.iterator();
        this.index = 0;
    }

    // ----- Instance methods -----

    protected Object computeNext() {
        if (!iterator.hasNext()) return null;

        final var keys = new String[] { "fst", "snd", Constants.STRUCT_TYPE_TAG };
        final var vals = new Object[] { index++, iterator.next(), "Pair" };
        return LKQLObject.createUncached(keys, vals);
    }
}
