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

package com.adacore.lkql_jit.built_ins.values.bases;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This class is the base for all "basic" LKQL values. */
@ExportLibrary(InteropLibrary.class)
public abstract class BasicLKQLValue implements LKQLValue {

    // ----- Attributes -----

    protected final InteropLibrary thisUncachedLibrary = InteropLibrary.getUncached(this);

    // ----- Value methods -----

    /** Tell the interop API that the value has an associated language. */
    @ExportMessage
    public boolean hasLanguage() {
        return true;
    }

    /** Give the LKQL language class to the interop library. */
    @ExportMessage
    public Class<? extends TruffleLanguage<?>> getLanguage() {
        return LKQLLanguage.class;
    }

    /**
     * Get the default displayable string, exporting because this message is abstract and all
     * classes which export the interop library must implement it.
     */
    @ExportMessage
    public String toDisplayString(@SuppressWarnings("unused") boolean allowSideEffects) {
        return "<basic_lkql_value>";
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return (String) thisUncachedLibrary.toDisplayString(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof BasicLKQLValue other)) return false;
        InteropLibrary otherUncached = InteropLibrary.getUncached(other);
        return thisUncachedLibrary.isIdentical(this, other, otherUncached);
    }

    @Override
    public int hashCode() {
        try {
            return thisUncachedLibrary.identityHashCode(this);
        } catch (UnsupportedMessageException e) {
            throw LKQLRuntimeException.shouldNotHappen(
                    "All LKQL values must export an 'identityHashCode' message");
        }
    }
}
