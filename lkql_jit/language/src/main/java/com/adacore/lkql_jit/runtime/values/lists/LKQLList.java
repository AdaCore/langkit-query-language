//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.lists;

import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.runtime.values.iterators.LKQLIterator;
import com.adacore.lkql_jit.runtime.values.iterators.LKQLListIterator;
import java.util.Arrays;

/** This class represents an array list in the LKQL language. */
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
}
