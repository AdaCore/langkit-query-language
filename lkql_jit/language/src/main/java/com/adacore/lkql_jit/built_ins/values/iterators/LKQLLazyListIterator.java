//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.values.iterators;

import com.adacore.lkql_jit.built_ins.values.lists.LKQLLazyList;
import com.adacore.lkql_jit.exception.utils.InvalidIndexException;

/** This class represents an iterator for an LKQL lazy list. */
public class LKQLLazyListIterator extends LKQLIterator {

    // ----- Attributes -----

    /** The lazy list to iterate on. */
    private final LKQLLazyList lazyList;

    /** The cursor for the iteration. */
    private long cursor;

    // ----- Constructors -----

    /** Create a new lazy list iterator for the given lazy list. */
    public LKQLLazyListIterator(LKQLLazyList lazyList) {
        this.lazyList = lazyList;
        this.cursor = 0;
    }

    // ----- Iterator required methods -----

    @Override
    public boolean hasNext() {
        try {
            this.lazyList.get(cursor);
            return true;
        } catch (InvalidIndexException e) {
            return false;
        }
    }

    @Override
    public Object next() {
        return this.lazyList.get(this.cursor++);
    }

    @Override
    public void reset() {
        this.cursor = 0;
    }
}
