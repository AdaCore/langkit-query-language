//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.lists;

import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.interop.LKQLIterator;
import com.adacore.lkql_jit.values.iterators.BaseLKQLListIterator;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This class represents the base of all LKQL lazy lists. */
@ExportLibrary(InteropLibrary.class)
public abstract class BaseLKQLLazyList extends BaseLKQLList {

    // ----- Attributes -----

    /** Cache to store elements of the lazy list */
    protected ListStorage<Object> cache;

    // ----- Constructors -----

    protected BaseLKQLLazyList(ListStorage<Object> cache) {
        this.cache = cache;
    }

    // ----- Instance methods -----

    /**
     * This method should initialize the element cache from the next lazy element to the provided
     * index.
     */
    protected abstract void initCacheTo(long n);

    // ----- List required methods -----

    @Override
    public Object get(long n) {
        this.initCacheTo(n);
        return this.cache.get((int) n);
    }

    @Override
    public long size() {
        this.initCacheTo(-1);
        return this.cache.size();
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
