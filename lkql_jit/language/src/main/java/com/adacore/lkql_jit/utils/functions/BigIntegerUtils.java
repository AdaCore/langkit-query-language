/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

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
     * Get a big integer from a long value.
     *
     * @param l The long value.
     * @return The big integer.
     */
    @CompilerDirectives.TruffleBoundary
    public static BigInteger valueOf(long l) {
        return BigInteger.valueOf(l);
    }

    /**
     * Get a big integer from an int.
     *
     * @param i The int value.
     * @return The big integer.
     */
    @CompilerDirectives.TruffleBoundary
    public static BigInteger valueOf(int i) {
        return BigInteger.valueOf(i);
    }

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
