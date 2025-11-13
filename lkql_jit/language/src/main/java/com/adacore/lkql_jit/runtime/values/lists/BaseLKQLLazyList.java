//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.lists;

import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.runtime.values.iterators.BaseLKQLListIterator;
import com.adacore.lkql_jit.runtime.values.iterators.LKQLIterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This class represents the base of all LKQL lazy lists. */
@ExportLibrary(InteropLibrary.class)
public abstract class BaseLKQLLazyList extends BaseLKQLList {

    // ----- Attributes -----

    /** The cache of the lazy list. */
    protected final ListStorage<Object> cache;

    // ----- Constructors -----

    protected BaseLKQLLazyList() {
        this.cache = new ListStorage<>(16);
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
    public Object get(long i) throws IndexOutOfBoundsException {
        this.computeItemAt(i);
        return this.cache.get((int) i);
    }

    @Override
    public LKQLIterator iterator() {
        return new BaseLKQLListIterator(this);
    }

    // ----- Value methods -----

    @ExportMessage
    public Object toDisplayString(@SuppressWarnings("unused") final boolean allowSideEffect) {
        return "<" + LKQLTypesHelper.LKQL_LAZY_LIST + ">";
    }
}
