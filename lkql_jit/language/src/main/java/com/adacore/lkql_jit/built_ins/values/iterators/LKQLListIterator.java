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

import com.adacore.lkql_jit.built_ins.values.lists.LKQLList;

/** This class represents an iterator on a list in the LKQL language. */
public final class LKQLListIterator extends LKQLIterator {

    // ----- Instance attributes -----

    /** The list to iterate on. */
    private final LKQLList list;

    /** The cursor to the next element to return. */
    private int cursor;

    // ----- Constructors -----

    /** Create a new list iterator with the iterated list. */
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
