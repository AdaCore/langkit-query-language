//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.values.iterators;

import com.adacore.lkql_jit.utils.Iterator;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This class represents an iterator value in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public abstract class LKQLIterator implements TruffleObject, Iterator {

    // ----- Constructors -----

    /** The protected constructor. */
    protected LKQLIterator() {}

    // ----- Iterator required methods -----

    /** Get whether the iterator has a next element. */
    public abstract boolean hasNext();

    /** Get the next element and move the cursor forward. */
    public abstract Object next();

    // ----- Value methods -----

    /** Tell the interop library that the value is an iterator. */
    @ExportMessage
    public boolean isIterator() {
        return true;
    }

    /** Get if the iterator has a next element. */
    @ExportMessage
    public boolean hasIteratorNextElement() {
        return this.hasNext();
    }

    /**
     * Get the next element of the iterator.
     *
     * @throws StopIterationException If there is no next element.
     */
    @ExportMessage
    public Object getIteratorNextElement() throws StopIterationException {
        try {
            return this.next();
        } catch (IndexOutOfBoundsException e) {
            throw StopIterationException.create(e);
        }
    }
}
