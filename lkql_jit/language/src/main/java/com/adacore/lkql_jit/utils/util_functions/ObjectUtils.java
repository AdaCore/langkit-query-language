package com.adacore.lkql_jit.utils.util_functions;

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
     * @param left  The left object.
     * @param right The right object.
     * @return The equality.
     */
    @CompilerDirectives.TruffleBoundary
    public static boolean equals(Object left, Object right) {
        return Objects.equals(left, right);
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
