//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.Collections;
import java.util.List;

/**
 * Util functions for the java generic list type.
 *
 * @author Hugo GUERRIER
 */
public final class ListUtils {

    @SafeVarargs
    @CompilerDirectives.TruffleBoundary
    public static <T> void addAll(List<T> lst, T... elems) {
        Collections.addAll(lst, elems);
    }
}
