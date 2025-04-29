//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.lists;

import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.runtime.values.iterators.LKQLIterator;
import com.adacore.lkql_jit.runtime.values.iterators.LKQLLazyListIterator;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.ArrayList;
import java.util.List;

/** This class represents the base of all LKQL lazy lists. */
@ExportLibrary(InteropLibrary.class)
public abstract class LKQLLazyList extends BaseLKQLList {

    // ----- Attributes -----

    /** The cache of the lazy list. */
    protected final List<Object> cache;

    // ----- Constructors -----

    protected LKQLLazyList() {
        this.cache = new ArrayList<>();
    }

    // ----- Lazy list required methods -----

    /**
     * Initialize the lazy list cache to the given index. If n < 0 then initialize all the lazy list
     * values.
     */
    public abstract void computeItemAt(long n);

    // ----- List required methods -----

    @Override
    public long size() {
        this.computeItemAt(-1);
        return this.cache.size();
    }

    @Override
    public Object get(long i) throws InvalidIndexException {
        this.computeItemAt(i);
        try {
            return this.cache.get((int) i);
        } catch (IndexOutOfBoundsException e) {
            throw new InvalidIndexException();
        }
    }

    @Override
    public LKQLIterator iterator() {
        return new LKQLLazyListIterator(this);
    }

    // ----- Value methods -----

    /** Return the identity hash code for the given LKQL lazy list. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public static int identityHashCode(LKQLLazyList receiver) {
        return System.identityHashCode(receiver);
    }

    @Override
    @ExportMessage
    public Object toDisplayString(
        @SuppressWarnings("unused") final boolean allowSideEffect,
        @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary elems
    ) {
        return "<" + LKQLTypesHelper.LKQL_LAZY_LIST + ">";
    }
}
