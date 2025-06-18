//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.runtime.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Nullish;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;

/** This class represents the unit value in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public class LKQLUnit extends BasicLKQLValue implements Nullish {

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
        @SuppressWarnings("unused") final LKQLUnit receiver,
        final Object other
    ) {
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
}
