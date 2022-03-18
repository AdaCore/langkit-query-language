/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.utils.util_functions;

import com.oracle.truffle.api.CompilerDirectives;

import java.lang.reflect.Array;
import java.util.*;


/**
 * Util functions to manipulate the arrays in the JIT implementation
 *
 * @author Hugo GUERRIER
 */
public final class ArrayUtils {

    /**
     * Get the first index of the first matching element in the array
     *
     * @param arr The array to search in
     * @param val The value to look for
     * @return The index of the first occurence of the value
     */
    @CompilerDirectives.TruffleBoundary
    public static <T> int indexOf(T[] arr, T val) {
        for(int i = 0 ; i < arr.length ; i++) {
            if(val.equals(arr[i])) return i;
        }
        return -1;
    }

    /**
     * Compare the two array
     *
     * @param left The left array
     * @param right The right array
     * @return The result of the comparison
     * @param <T> The type of the array elements
     */
    @CompilerDirectives.TruffleBoundary
    public static <T> boolean equals(T[] left, T[] right) {
        // Compare the length
        if(left.length != right.length) return false;

        // Compare the elements
        for(int i = 0 ; i < left.length ; i++) {
            if(!Objects.equals(left[i], right[i])) return false;
        }

        // Return the success
        return true;
    }

    /**
     * Concatenate two array of arbitrary types
     *
     * @param left The left array
     * @param right The right array
     * @param <T> The array type
     * @return The concatenation of the arrays in a new array
     */
    public static <T> T[] concat(T[] left, T[] right) {
        T[] res = Arrays.copyOf(left, left.length + right.length);
        System.arraycopy(right, 0, res, left.length, right.length);
        return res;
    }

    /**
     * Create an array where duplicate entries of input are deleted
     *
     * @param array The input array
     * @param <T> Type of the array
     * @return The unique array
     */
    @CompilerDirectives.TruffleBoundary
    public static <T> T[] unique(T[] array) {
        ArrayList<T> resList = new ArrayList<>();
        for(T elem : array) {
            if(!resList.contains(elem)) {
                resList.add(elem);
            }
        }
        return (T[]) resList.toArray();
    }

    /**
     * Get the string representation of a given array
     *
     * @param array The array to get the representation from
     * @return The string representation of the array
     * @param <T> The type of the array elements
     */
    @CompilerDirectives.TruffleBoundary
    public static <T> String toString(T[] array) {
        return Arrays.toString(array);
    }

}
