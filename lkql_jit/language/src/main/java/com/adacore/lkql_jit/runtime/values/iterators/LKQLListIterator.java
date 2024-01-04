//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.iterators;

import com.adacore.lkql_jit.runtime.values.lists.LKQLList;

/** This class represents an iterator on a list in the LKQL language. */
public final class LKQLListIterator extends LKQLIterator {

    // ----- Instance attributes -----

    /** The list to iterate on. */
    private final LKQLList list;

    /** The cursor to the next element to return. */
    private int cursor;

    // ----- Constructors -----

    /** Create a new list iterator for the given list. */
    public LKQLListIterator(final LKQLList list) {
        this.list = list;
        this.cursor = 0;
    }

    // ----- Iterator required methods -----

    @Override
    public boolean hasNext() {
        return this.cursor < this.list.size();
    }

    @Override
    public Object next() {
        return this.list.get(this.cursor++);
    }

    @Override
    public void reset() {
        this.cursor = 0;
    }
}
