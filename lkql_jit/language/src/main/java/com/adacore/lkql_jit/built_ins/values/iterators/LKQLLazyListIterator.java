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
