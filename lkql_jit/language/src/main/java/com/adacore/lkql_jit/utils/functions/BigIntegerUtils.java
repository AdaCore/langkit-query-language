//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.oracle.truffle.api.CompilerDirectives;
import java.math.BigInteger;

/** Util functions for to manipulate big integers with Truffle boundaries. */
public final class BigIntegerUtils {

    /** Get the negative value of the given big integer. */
    @CompilerDirectives.TruffleBoundary
    public static BigInteger negate(BigInteger arg) {
        return arg.negate();
    }

    /** Add two big integers. */
    @CompilerDirectives.TruffleBoundary
    public static BigInteger add(BigInteger left, BigInteger right) {
        return left.add(right);
    }

    /** Subtract a big integer to another. */
    @CompilerDirectives.TruffleBoundary
    public static BigInteger subtract(BigInteger left, BigInteger right) {
        return left.subtract(right);
    }

    /** Multiply two big integers. */
    @CompilerDirectives.TruffleBoundary
    public static BigInteger multiply(BigInteger left, BigInteger right) {
        return left.multiply(right);
    }

    /** Divide two big integers. */
    @CompilerDirectives.TruffleBoundary
    public static BigInteger divide(BigInteger left, BigInteger right) {
        return left.divide(right);
    }

    /** Get whether the left big integer is equals to the right one. */
    @CompilerDirectives.TruffleBoundary
    public static boolean equals(BigInteger left, BigInteger right) {
        return left.equals(right);
    }

    /** Compare two big integers and return the result. */
    @CompilerDirectives.TruffleBoundary
    public static int compareTo(BigInteger left, BigInteger right) {
        return left.compareTo(right);
    }

    /** Get the int value of a big integer. */
    @CompilerDirectives.TruffleBoundary
    public static int intValue(BigInteger biggie) {
        return biggie.intValue();
    }
}
