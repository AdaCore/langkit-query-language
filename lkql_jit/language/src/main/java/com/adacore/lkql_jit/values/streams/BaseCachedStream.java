//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.streams;

import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
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
     * Computes the next element of the stream and returns it.
     * Returns null if nothing left to compute.
     */
    protected abstract Object computeNext();

    /** Initialize cache until the given index and returns the result. */
    public Object get(long n) {
        while (n >= this.cache.size()) {
            var next = computeNext();
            if (next == null) break;
            cache.append(next);
        }
        if (cache.size() <= n) return null;
        return this.cache.get((int) n);
    }

    public Object getHead() {
        Object res = null;
        try {
            res = this.get(0);
        } catch (IndexOutOfBoundsException _) {}

        if (res == null) throw LKQLRuntimeError.emptyStreamHead(null);

        return res;
    }

    @Override
    public LKQLStream getTail() {
        boolean empty = true;
        try {
            empty = this.get(0) == null;
        } catch (IndexOutOfBoundsException _) {}

        if (empty) throw LKQLRuntimeError.emptyStreamTail(null);

        return new OffsetStream(this, 1);
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
            Object res = null;
            try {
                res = base.get(offset);
            } catch (IndexOutOfBoundsException _) {}

            if (res == null) throw LKQLRuntimeError.emptyStreamHead(null);

            return res;
        }

        @Override
        public LKQLStream getTail() {
            boolean empty = true;
            try {
                empty = base.get(offset) == null;
            } catch (IndexOutOfBoundsException _) {}

            if (empty) throw LKQLRuntimeError.emptyStreamTail(null);

            return new OffsetStream(base, offset + 1);
        }
    }
}
