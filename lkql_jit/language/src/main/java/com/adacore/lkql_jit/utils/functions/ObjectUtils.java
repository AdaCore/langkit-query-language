//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.Objects;

/** Util functions for the Java object, in Truffle boundaries. */
public final class ObjectUtils {

    /** Check the equality between two objects. */
    @CompilerDirectives.TruffleBoundary
    public static boolean equals(Object left, Object right) {
        return Objects.equals(left, right);
    }

    /** Get the hash code of an object. */
    @CompilerDirectives.TruffleBoundary
    public static int hashCode(Object o) {
        return o.hashCode();
    }

    /** Get the string representation of the object, calling the `toString` method on it. */
    @CompilerDirectives.TruffleBoundary
    public static String toString(Object o) {
        return o == null ? "null" : o.toString();
    }
}
