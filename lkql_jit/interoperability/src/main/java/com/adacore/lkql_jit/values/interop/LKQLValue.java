//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interop;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/**
 * This class is the base for all LKQL values, except for object-like ones
 * (see @see com.adacore.lkql_jit.values.interop.LKQLDynamicObject).
 */
@ExportLibrary(InteropLibrary.class)
public abstract class LKQLValue implements TruffleObject {

    // ----- Value methods -----

    @ExportMessage
    public String toDisplayString(@SuppressWarnings("unused") boolean allowSideEffects) {
        return "<lkql_value>";
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return (String) InteropLibrary.getUncached().toDisplayString(this);
    }
}
