//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.streams;

import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.values.interfaces.Iterator;
import com.adacore.lkql_jit.values.interop.LKQLStream;
import com.adacore.lkql_jit.values.iterators.LKQLStreamIterator;

/**
 * This class represents a static list viewed as a stream.
 */
public class LKQLArrayStream extends LKQLStream {

    // ----- Attributes -----

    public final Object[] content;

    // ----- Constructors -----

    public LKQLArrayStream(Object[] content) {
        this.content = content;
    }

    // ----- Instance methods -----

    @Override
    public Iterator iterator() {
        return new LKQLStreamIterator(this);
    }

    @Override
    public Object get(long index) throws IndexOutOfBoundsException {
        return content[(int) index];
    }

    @Override
    public Object getHead() {
        if (content.length == 0) throw LKQLRuntimeError.emptyStreamHead(null);
        return content[0];
    }

    @Override
    public LKQLStream getTail() {
        if (content.length == 0) throw LKQLRuntimeError.emptyStreamTail(null);
        return new OffsetArrayStream(content, 1);
    }

    // ----- Inner classes -----

    private static class OffsetArrayStream extends LKQLArrayStream {

        private final int offset;

        private OffsetArrayStream(Object[] content, int offset) {
            super(content);
            this.offset = offset;
        }

        @Override
        public Iterator iterator() {
            return new LKQLStreamIterator(this);
        }

        @Override
        public Object get(long index) throws IndexOutOfBoundsException {
            return content[(int) index + offset];
        }

        @Override
        public Object getHead() {
            if (content.length < offset) throw LKQLRuntimeError.emptyStreamHead(null);
            return content[offset];
        }

        @Override
        public LKQLStream getTail() {
            if (content.length < offset) throw LKQLRuntimeError.emptyStreamTail(null);
            return new OffsetArrayStream(content, offset + 1);
        }
    }
}
