//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.oracle.truffle.api.CompilerDirectives;
import java.math.BigInteger;

/**
 * Util functions for the big integers.
 *
 * @author Hugo GUERRIER
 */
public final class BigIntegerUtils {

    /**
     * Get the negative value of the given big integer.
     *
     * @param arg The big integer to negate.
     * @return The negated big integer.
     */
    @CompilerDirectives.TruffleBoundary
    public static BigInteger negate(BigInteger arg) {
        return arg.negate();
    }

    /**
     * Add two big integers.
     *
     * @param left The left operand.
     * @param right The right operand.
     * @return The result of the addition.
     */
    @CompilerDirectives.TruffleBoundary
    public static BigInteger add(BigInteger left, BigInteger right) {
        return left.add(right);
    }

    /**
     * Subtract a big integer to another big integer.
     *
     * @param left The left big integer.
     * @param right The right big integer.
     * @return The result of the subtraction.
     */
    @CompilerDirectives.TruffleBoundary
    public static BigInteger subtract(BigInteger left, BigInteger right) {
        return left.subtract(right);
    }

    /**
     * Multiply two big integers.
     *
     * @param left The left operand.
     * @param right The right operand.
     * @return The result of the multiplication.
     */
    @CompilerDirectives.TruffleBoundary
    public static BigInteger multiply(BigInteger left, BigInteger right) {
        return left.multiply(right);
    }

    /**
     * Divide two big integers.
     *
     * @param left The left operand.
     * @param right The right operand.
     * @return The result of the division.
     */
    @CompilerDirectives.TruffleBoundary
    public static BigInteger divide(BigInteger left, BigInteger right) {
        return left.divide(right);
    }

    /**
     * Get if the left big integer is equals to the right one.
     *
     * @param left The left big integer.
     * @param right The right big integer.
     * @return If those are equals.
     */
    @CompilerDirectives.TruffleBoundary
    public static boolean equals(BigInteger left, BigInteger right) {
        return left.equals(right);
    }

    /**
     * Get the result of the comparison of the two big integers.
     *
     * @param left The left one.
     * @param right The right one.
     * @return The result of the comparison.
     */
    @CompilerDirectives.TruffleBoundary
    public static int compareTo(BigInteger left, BigInteger right) {
        return left.compareTo(right);
    }

    /**
     * Get the int value of a big integer.
     *
     * @param biggie The big integer.
     * @return The value in an integer.
     */
    @CompilerDirectives.TruffleBoundary
    public static int intValue(BigInteger biggie) {
        return biggie.intValue();
    }
}
