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

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.built_ins.values.interfaces.Nullish;
import com.adacore.lkql_jit.built_ins.values.interfaces.Truthy;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;

/** This class represents the null value in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLNull extends Libadalang.AdaNode implements TruffleObject, Nullish, Truthy {

    // ----- Attributes -----

    /** The sole instance of the LKQL null value. */
    public static final LKQLNull INSTANCE = new LKQLNull();

    /** The identity hash of the only instance of the LKQL null. */
    private static final int IDENTITY_HASH = System.identityHashCode(INSTANCE);

    // ----- Constructors -----

    /** Private constructor for the null value. */
    private LKQLNull() {
        super(
                Libadalang.Entity.create(
                        Libadalang.PointerWrapper.nullPointer(),
                        Libadalang.EntityInfo.create(
                                Libadalang.Metadata.create(
                                        false,
                                        Libadalang.PointerWrapper.nullPointer(),
                                        Libadalang.PointerWrapper.nullPointer()),
                                Libadalang.PointerWrapper.nullPointer(),
                                false)));
    }

    // ----- Node methods -----

    @Override
    public boolean isNone() {
        return true;
    }

    // ----- Value methods -----

    /** Tell the interop API that the value has an associated language. */
    @ExportMessage
    boolean hasLanguage() {
        return true;
    }

    /** Give the LKQL language class to the interop library. */
    @ExportMessage
    Class<? extends TruffleLanguage<?>> getLanguage() {
        return LKQLLanguage.class;
    }

    /** Tell the interop API if the given other value is null. */
    @ExportMessage
    static TriState isIdenticalOrUndefined(
            @SuppressWarnings("unused") final LKQLNull receiver, final Object other) {
        return TriState.valueOf(receiver == other);
    }

    /**
     * Return the identity hash code for the given receiver (always the same because null value is
     * singleton).
     */
    @ExportMessage
    static int identityHashCode(@SuppressWarnings("unused") final LKQLNull receiver) {
        return IDENTITY_HASH;
    }

    /** Get the displayable string for the interop library. */
    @ExportMessage
    Object toDisplayString(@SuppressWarnings("unused") boolean allowSideEffect) {
        return "null";
    }

    /** Tell the interop API that the value is nullish. */
    @ExportMessage
    boolean isNull() {
        return true;
    }

    /** Tell the interop API that the value is a boolean like value. */
    @ExportMessage
    boolean isBoolean() {
        return true;
    }

    /** Get the boolean like value from null, which is always false. */
    @ExportMessage
    boolean asBoolean() {
        return false;
    }

    // ----- LKQL value methods -----

    @Override
    public boolean internalEquals(LKQLValue o) {
        return o == this;
    }

    @Override
    public boolean isTruthy() {
        return false;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return "null";
    }

    @Override
    public boolean equals(Object o) {
        return o == this;
    }
}
