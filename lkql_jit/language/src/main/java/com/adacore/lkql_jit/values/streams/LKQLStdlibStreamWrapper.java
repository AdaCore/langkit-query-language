//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.streams;

import com.adacore.lkql_jit.runtime.ListStorage;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import java.util.Iterator;
import java.util.stream.Stream;

/**
 * This class is a wrapper from a Java Stream to an LKQLStream.
 * It is currently only used to wrap the units() builtin.
 */
public class LKQLStdlibStreamWrapper extends BaseCachedStream {

    // ----- Attributes -----

    private Iterator<? extends Object> iterator;

    // ----- Constructors -----

    public LKQLStdlibStreamWrapper(Stream<? extends Object> source) {
        super(new ListStorage<>(0));
        iterator = source.iterator();
    }

    // ----- Instance methods -----

    @TruffleBoundary
    protected Object computeNext() {
        return iterator.hasNext() ? iterator.next() : null;
    }
}
