//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * Util functions to manipulate the arrays in the JIT implementation.
 *
 * @author Hugo GUERRIER
 */
public final class ArrayUtils {

    /**
     * Get the first index of the first matching element in the array.
     *
     * @param arr The array to search in.
     * @param val The value to look for.
     * @return The index of the first occurrence of the value.
     */
    @CompilerDirectives.TruffleBoundary
    public static <T> int indexOf(T[] arr, T val) {
        for (int i = 0; i < arr.length; i++) {
            if (val.equals(arr[i])) return i;
        }
        return -1;
    }

    /**
     * Compare the two array.
     *
     * @param left The left array.
     * @param right The right array.
     * @param <T> The type of the array elements.
     * @return The result of the comparison.
     */
    @CompilerDirectives.TruffleBoundary
    public static <T> boolean equals(T[] left, T[] right) {
        // Compare the length
        if (left.length != right.length) return false;

        // Compare the elements
        for (int i = 0; i < left.length; i++) {
            if (!Objects.equals(left[i], right[i])) return false;
        }

        // Return the success
        return true;
    }

    /**
     * Concatenate two array of arbitrary types.
     *
     * @param left The left array.
     * @param right The right array.
     * @param <T> The array type.
     * @return The concatenation of the arrays in a new array.
     */
    public static <T> T[] concat(T[] left, T[] right) {
        T[] res = Arrays.copyOf(left, left.length + right.length);
        System.arraycopy(right, 0, res, left.length, right.length);
        return res;
    }

    /**
     * Create an array where duplicate entries of input are deleted.
     *
     * @param array The input array.
     * @param <T> Type of the array.
     * @return The unique array.
     */
    @CompilerDirectives.TruffleBoundary
    public static <T> List<T> unique(T[] array) {
        ArrayList<T> resList = new ArrayList<>();
        for (T elem : array) {
            if (!resList.contains(elem)) {
                resList.add(elem);
            }
        }
        return resList;
    }

    /**
     * Get the string representation of a given array.
     *
     * @param array The array to get the representation from.
     * @param <T> The type of the array elements.
     * @return The string representation of the array.
     */
    @CompilerDirectives.TruffleBoundary
    public static <T> String toString(T[] array) {
        return Arrays.toString(array);
    }
}
