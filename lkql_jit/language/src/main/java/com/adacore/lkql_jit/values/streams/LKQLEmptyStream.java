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
 * This class represents the Nil stream.
 */
public class LKQLEmptyStream extends LKQLStream {

    // ----- Attributes -----

    /** Singleton instance of an empty stream. */
    public static final LKQLEmptyStream INSTANCE = new LKQLEmptyStream();

    // ----- Constructors -----

    private LKQLEmptyStream() {}

    // ----- Instance methods -----

    @Override
    public Iterator iterator() {
        return new LKQLStreamIterator(this);
    }

    @Override
    public Object get(long index) throws IndexOutOfBoundsException {
        throw new IndexOutOfBoundsException();
    }

    @Override
    public Object getHead() {
        throw LKQLRuntimeError.emptyStreamHead(null);
    }

    @Override
    public LKQLStream getTail() {
        throw LKQLRuntimeError.emptyStreamTail(null);
    }
}
