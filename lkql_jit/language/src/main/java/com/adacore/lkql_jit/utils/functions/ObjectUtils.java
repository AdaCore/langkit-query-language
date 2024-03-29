//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.Objects;

/**
 * Util functions for the java object type.
 *
 * @author Hugo GUERRIER
 */
public final class ObjectUtils {

    /**
     * Verify the equality between two object.
     *
     * @param left The left object.
     * @param right The right object.
     * @return The equality.
     */
    @CompilerDirectives.TruffleBoundary
    public static boolean equals(Object left, Object right) {
        return Objects.equals(left, right);
    }

    /** Bounded method to access an object hash code. */
    @CompilerDirectives.TruffleBoundary
    public static int hashCode(Object o) {
        return o.hashCode();
    }

    /**
     * Get the string representation of the object.
     *
     * @param o The object to get the representation from.
     * @return The string representation of the object.
     */
    @CompilerDirectives.TruffleBoundary
    public static String toString(Object o) {
        return o == null ? "null" : o.toString();
    }
}
