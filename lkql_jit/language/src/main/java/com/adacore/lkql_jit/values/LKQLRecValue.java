//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values;

import com.adacore.lkql_jit.values.interop.LKQLValue;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.Arrays;

@ExportLibrary(InteropLibrary.class)
public class LKQLRecValue extends LKQLValue {

    public final Object[] recurseVal;
    public final Object[] resultVal;

    public int depth;

    public LKQLRecValue(Object[] recurseVal, Object[] resultVal) {
        this.recurseVal = recurseVal;
        this.resultVal = resultVal;
        this.depth = -1;
    }

    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public String toDisplayString(@SuppressWarnings("unused") boolean allowSideEffects) {
        return Arrays.toString(this.recurseVal) + "::" + Arrays.toString(this.resultVal);
    }
}
