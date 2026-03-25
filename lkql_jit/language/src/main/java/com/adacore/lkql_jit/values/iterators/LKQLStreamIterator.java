//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.iterators;

import com.adacore.lkql_jit.values.interop.LKQLIterator;
import com.adacore.lkql_jit.values.interop.LKQLStream;

/** This class represents an iterator for an LKQL stream. */
public class LKQLStreamIterator extends LKQLIterator {

    // ----- Attributes -----

    /** The stream to iterate on. */
    private final LKQLStream stream;

    /** The cursor for the iteration. */
    private long cursor;

    // ----- Constructors -----

    /** Create a new stream iterator for the given stream. */
    public LKQLStreamIterator(LKQLStream stream) {
        this.stream = stream;
        this.cursor = 0;
    }

    // ----- Iterator required methods -----

    @Override
    public boolean hasNext() {
        try {
            this.stream.get(cursor);
            return true;
        } catch (IndexOutOfBoundsException e) {
            return false;
        }
    }

    @Override
    public Object next() {
        return this.stream.get(this.cursor++);
    }

    @Override
    public void reset() {
        this.cursor = 0;
    }
}
