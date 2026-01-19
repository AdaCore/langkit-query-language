//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.lists;

import com.adacore.lkql_jit.runtime.ListStorage;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import java.util.Iterator;
import java.util.stream.Stream;

/**
 * This class is a wrapper from a Java Stream to an LKQLLazyList.
 * It is currently only used to wrap the units() builtin.
 */
public class LKQLLazyListStreamWrapper extends BaseLKQLLazyList {

    // ----- Attributes -----

    private Iterator<? extends Object> iterator;

    // ----- Constructors -----

    public LKQLLazyListStreamWrapper(Stream<? extends Object> source) {
        super(new ListStorage<>(0));
        iterator = source.iterator();
    }

    // ----- Instance methods -----

    @TruffleBoundary
    protected void initCacheTo(long n) {
        while ((n < 0 || cache.size() <= n) && iterator.hasNext()) {
            cache.append(iterator.next());
        }
    }
}
