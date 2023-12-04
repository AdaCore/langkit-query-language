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

package com.adacore.lkql_jit.built_ins.values;

import com.adacore.lkql_jit.built_ins.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.built_ins.values.interfaces.Nullish;
import com.adacore.lkql_jit.built_ins.values.interfaces.Truthy;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;

/** This class represents the unit value in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public class LKQLUnit extends BasicLKQLValue implements Truthy, Nullish {

    // ----- Attributes -----

    /** The only instance of the unit value. */
    public static final LKQLUnit INSTANCE = new LKQLUnit();

    /** The identity hash of the only instance of the LKQL unit. */
    private static final int IDENTITY_HASH = System.identityHashCode(INSTANCE);

    // ----- Constructors -----

    /** The private constructor. */
    private LKQLUnit() {}

    // ----- Value methods -----

    /** Tell the interop API if the receiver is identical to the other. */
    @ExportMessage
    public static TriState isIdenticalOrUndefined(
            @SuppressWarnings("unused") final LKQLUnit receiver, final Object other) {
        return TriState.valueOf(receiver == other);
    }

    /**
     * Return the identity hash code for the given receiver (always the same because unit value is
     * singleton).
     */
    @ExportMessage
    public static int identityHashCode(@SuppressWarnings("unused") final LKQLUnit receiver) {
        return IDENTITY_HASH;
    }

    /** Get the displayable string for the interop library. */
    @Override
    @ExportMessage
    public String toDisplayString(@SuppressWarnings("unused") boolean allowSideEffect) {
        return "()";
    }

    /** Tell the interop API that the value is nullish. */
    @ExportMessage
    public boolean isNull() {
        return true;
    }

    /** Tell the interop API that the value is a boolean like value. */
    @ExportMessage
    public boolean isBoolean() {
        return true;
    }

    /** Get the boolean like value from unit, which is always false. */
    @ExportMessage
    public boolean asBoolean() {
        return false;
    }

    // ----- LKQL value methods -----

    @Override
    public boolean isTruthy() {
        return false;
    }
}
