//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.lists;

import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.runtime.values.iterators.LKQLIterator;
import com.adacore.lkql_jit.runtime.values.iterators.LKQLListIterator;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.Arrays;

/** This class represents an array list in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public final class LKQLList extends BaseLKQLList {

    // ----- Attributes -----

    /** The content of the array list. */
    public final Object[] content;

    // ----- Constructors -----

    /** Create a new array list with its content. */
    public LKQLList(final Object[] content) {
        this.content = content;
    }

    // ----- List required methods -----

    @Override
    public long size() {
        return this.content.length;
    }

    @Override
    public Object get(long i) throws InvalidIndexException {
        try {
            return this.content[(int) i];
        } catch (IndexOutOfBoundsException e) {
            throw new InvalidIndexException();
        }
    }

    public Object[] getSlice(long first, long last) throws InvalidIndexException {
        try {
            return Arrays.copyOfRange(this.content, (int) first, (int) last);
        } catch (IndexOutOfBoundsException e) {
            throw new InvalidIndexException();
        }
    }

    @Override
    public LKQLIterator iterator() {
        return new LKQLListIterator(this);
    }

    @Override
    public Object[] getContent() {
        return this.content;
    }

    // ----- Value methods -----

    /** Return the identity hash code for the given LKQL array list. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public static int identityHashCode(LKQLList receiver) {
        return System.identityHashCode(receiver);
    }
}
