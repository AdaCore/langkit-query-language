//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.iterators;

import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.runtime.values.lists.BaseLKQLList;

/** This class represents an iterator for an LKQL lazy list. */
public class BaseLKQLListIterator extends LKQLIterator {

    // ----- Attributes -----

    /** The lazy list to iterate on. */
    private final BaseLKQLList lkqlList;

    /** The cursor for the iteration. */
    private long cursor;

    // ----- Constructors -----

    /** Create a new lazy list iterator for the given lazy list. */
    public BaseLKQLListIterator(BaseLKQLList lkqlList) {
        this.lkqlList = lkqlList;
        this.cursor = 0;
    }

    // ----- Iterator required methods -----

    @Override
    public boolean hasNext() {
        try {
            this.lkqlList.get(cursor);
            return true;
        } catch (InvalidIndexException e) {
            return false;
        }
    }

    @Override
    public Object next() {
        return this.lkqlList.get(this.cursor++);
    }

    @Override
    public void reset() {
        this.cursor = 0;
    }
}
