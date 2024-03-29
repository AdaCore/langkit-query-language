//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.List;

/**
 * Util functions for the java generic list type.
 *
 * @author Hugo GUERRIER
 */
public final class ListUtils {

    /**
     * Get if a list contains an element.
     *
     * @param list The list to verify.
     * @param elem The element to find.
     * @param <T> The type of the element.
     * @return True if the list contains the element.
     */
    @CompilerDirectives.TruffleBoundary
    public static <T> boolean contains(List<T> list, T elem) {
        return list.contains(elem);
    }
}
