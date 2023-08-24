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

import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This class represents an array list in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public final class LKQLArrayList extends LKQLList {

    // ----- Attributes -----

    /** The content of the array list. */
    private final Object[] content;

    // ----- Constructors -----

    /** Create a new array list with its content. */
    public LKQLArrayList(final Object[] content) {
        this.content = content;
    }

    // ----- List required methods -----

    @Override
    public long size() {
        return this.content.length;
    }

    @Override
    public Object get(int i) throws InvalidIndexException {
        try {
            return this.content[i];
        } catch (IndexOutOfBoundsException e) {
            throw new InvalidIndexException();
        }
    }

    @Override
    public Object[] asArray() {
        return this.content;
    }

    // ----- Value methods -----

    /** Return the identity hash code for the given LKQL array list. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public static int identityHashCode(LKQLArrayList receiver) {
        return System.identityHashCode(receiver);
    }
}
