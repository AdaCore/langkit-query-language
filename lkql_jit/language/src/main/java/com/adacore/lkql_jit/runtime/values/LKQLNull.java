//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Nullish;
import com.adacore.lkql_jit.runtime.values.interfaces.Truthy;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;

/** This class represents the null value in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLNull implements LangkitSupport.NodeInterface, LKQLValue, Nullish, Truthy {

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
                        Libadalang.PointerWrapper.nullPointer()
                    ),
                    Libadalang.PointerWrapper.nullPointer(),
                    false
                )
            )
        );
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
        @SuppressWarnings("unused") final LKQLNull receiver,
        final Object other
    ) {
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
