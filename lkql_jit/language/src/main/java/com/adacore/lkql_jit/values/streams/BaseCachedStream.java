//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.streams;

import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.values.interfaces.Iterator;
import com.adacore.lkql_jit.values.interop.LKQLIterator;
import com.adacore.lkql_jit.values.interop.LKQLStream;
import com.adacore.lkql_jit.values.iterators.LKQLStreamIterator;

/** This class represents the base of all LKQL cached streams. */
public abstract class BaseCachedStream extends LKQLStream {

    // ----- Attributes -----

    /** Cache to store elements of the stream. */
    protected ListStorage<Object> cache;

    // ----- Constructors -----

    protected BaseCachedStream(ListStorage<Object> cache) {
        this.cache = cache;
    }

    // ----- Instance methods -----

    /**
     * This method should initialize the element cache from the next lazy element to the provided
     * index.
     */
    protected abstract void initCacheTo(long n);

    public Object getHead() {
        return this.get(0);
    }

    @Override
    public LKQLStream getTail() {
        return new OffsetStream(this, 1);
    }

    @Override
    public Object get(long n) {
        this.initCacheTo(n);
        return this.cache.get((int) n);
    }

    @Override
    public LKQLIterator iterator() {
        return new LKQLStreamIterator(this);
    }

    // ----- Inner classes -----

    private static class OffsetStream extends LKQLStream {

        private final LKQLStream base;
        private final long offset;

        private OffsetStream(LKQLStream base, long offset) {
            this.base = base;
            this.offset = offset;
        }

        @Override
        public Iterator iterator() {
            return new LKQLStreamIterator(this);
        }

        @Override
        public Object get(long index) throws IndexOutOfBoundsException {
            return base.get(index + offset);
        }

        @Override
        public Object getHead() {
            return base.get(offset);
        }

        @Override
        public LKQLStream getTail() {
            return new OffsetStream(base, offset + 1);
        }
    }
}
