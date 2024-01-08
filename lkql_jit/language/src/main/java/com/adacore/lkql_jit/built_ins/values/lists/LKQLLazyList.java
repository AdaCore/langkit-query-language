/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.built_ins.values.lists;

import com.adacore.lkql_jit.built_ins.values.iterators.LKQLIterator;
import com.adacore.lkql_jit.built_ins.values.iterators.LKQLLazyListIterator;
import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
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
}
