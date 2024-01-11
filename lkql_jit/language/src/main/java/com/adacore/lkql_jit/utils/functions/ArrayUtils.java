//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.*;

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
     * From the given array, create a map associating each distinct element of the array to a list
     * containing indexes that refer to this element.
     */
    @CompilerDirectives.TruffleBoundary
    public static <T> Map<T, List<Integer>> indexMap(T[] arr) {
        final Map<T, List<Integer>> res = new HashMap<>();
        for (int i = 0; i < arr.length; i++) {
            final var elem = arr[i];
            final List<Integer> indexes = res.getOrDefault(elem, new ArrayList<>());
            indexes.add(i);
            res.put(elem, indexes);
        }
        return res;
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

    @CompilerDirectives.TruffleBoundary
    public static <T> String toString(T[] array) {
        return Arrays.toString(array);
    }
}
