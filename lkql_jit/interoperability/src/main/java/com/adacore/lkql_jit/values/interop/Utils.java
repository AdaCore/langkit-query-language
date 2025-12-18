//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interop;

import com.oracle.truffle.api.CompilerDirectives;

/** This class contains util functions used by interop values. */
public final class Utils {

    /** Util function to compare values in Truffle inspectable code. */
    @CompilerDirectives.TruffleBoundary
    public static boolean eq(Object left, Object right) {
        return left.equals(right);
    }

    /** Util function to get value hash code in Truffle inspectable code. */
    @CompilerDirectives.TruffleBoundary
    public static int hashCode(Object o) {
        return o.hashCode();
    }

    /** Util function to prepare a string for display purposes. */
    @CompilerDirectives.TruffleBoundary
    public static String toRepr(String source) {
        return ("\"" + source.replace("\"", "\\\"").replace("\n", "\\x0a") + "\"");
    }
}
