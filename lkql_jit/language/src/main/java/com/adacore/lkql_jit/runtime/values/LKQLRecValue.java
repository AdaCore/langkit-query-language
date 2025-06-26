//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.runtime.values.bases.BasicLKQLValue;
import com.oracle.truffle.api.CompilerDirectives;
import java.util.Arrays;

public class LKQLRecValue extends BasicLKQLValue {

    public final Object[] recurseVal;
    public final Object[] resultVal;

    public int depth;

    public LKQLRecValue(Object[] recurseVal, Object[] resultVal) {
        this.recurseVal = recurseVal;
        this.resultVal = resultVal;
        this.depth = -1;
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toDisplayString(@SuppressWarnings("unused") boolean allowSideEffects) {
        return Arrays.toString(this.recurseVal) + "::" + Arrays.toString(this.resultVal);
    }
}
