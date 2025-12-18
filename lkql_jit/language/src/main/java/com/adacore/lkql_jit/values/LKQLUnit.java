//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values;

import com.adacore.lkql_jit.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.values.interfaces.Nullish;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

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

    // ----- Override methods -----

    @Override
    public boolean equals(Object o) {
        return this == o;
    }

    @Override
    public int hashCode() {
        return IDENTITY_HASH;
    }
}
