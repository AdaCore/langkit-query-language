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

import com.adacore.lkql_jit.utils.Iterator;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This class represents an iterator value in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public abstract class LKQLIterator implements TruffleObject, Iterator {

    // ----- Constructors -----

    /** The protected constructor. */
    protected LKQLIterator() {}

    // ----- Iterator required methods -----

    /** Get whether the iterator has a next element. */
    public abstract boolean hasNext();

    /** Get the next element and move the cursor forward. */
    public abstract Object next();

    // ----- Value methods -----

    /** Tell the interop library that the value is an iterator. */
    @ExportMessage
    public boolean isIterator() {
        return true;
    }

    /** Get if the iterator has a next element. */
    @ExportMessage
    public boolean hasIteratorNextElement() {
        return this.hasNext();
    }

    /**
     * Get the next element of the iterator.
     *
     * @throws StopIterationException If there is no next element.
     */
    @ExportMessage
    public Object getIteratorNextElement() throws StopIterationException {
        try {
            return this.next();
        } catch (IndexOutOfBoundsException e) {
            throw StopIterationException.create(e);
        }
    }
}
