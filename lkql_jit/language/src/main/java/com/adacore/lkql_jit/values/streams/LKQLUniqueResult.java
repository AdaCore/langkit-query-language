//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.streams;

import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.values.interfaces.Iterable;
import com.adacore.lkql_jit.values.interfaces.Iterator;
import com.oracle.truffle.api.CompilerDirectives;
import java.util.Objects;

/** This class represents the result of a unique operation on a stream. */
public class LKQLUniqueResult extends BaseCachedStream {

    // ----- Attributes -----

    /** Collection that is mapped. */
    private final Iterator iterator;

    // ----- Constructors -----

    public LKQLUniqueResult(Iterable generator) {
        super(new ListStorage<>(16));
        this.iterator = generator.iterator();
    }

    // ----- Instance methods -----

    @CompilerDirectives.TruffleBoundary
    protected void initCacheTo(long n) {
        while (this.iterator.hasNext() && (n >= this.cache.size() || n < 0)) {
            var x = iterator.next();
            var found = false;
            for (int i = 0; i < cache.size(); i++) {
                if (Objects.equals(x, cache.get(i))) {
                    found = true;
                    break;
                }
            }
            if (!found) this.cache.append(x);
        }
    }
}
