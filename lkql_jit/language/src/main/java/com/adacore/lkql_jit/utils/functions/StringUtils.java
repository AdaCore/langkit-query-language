//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.adacore.lkql_jit.values.interop.Utils;
import com.oracle.truffle.api.CompilerDirectives;
import java.io.File;
import java.util.Locale;

/**
 * Util functions to manipulate the java string type in the JIT implementation.
 *
 * @author Hugo GUERRIER
 */
public final class StringUtils {

    // ----- Class methods -----

    /**
     * Concatenate given strings.
     *
     * @param strings The strings to concatenate.
     * @return The concatenated strings.
     */
    @CompilerDirectives.TruffleBoundary
    public static String concat(String... strings) {
        StringBuilder builder = new StringBuilder();
        for (String s : strings) {
            builder.append(s);
        }
        return builder.toString();
    }

    /**
     * Get if a string contains a given target.
     *
     * @param place The place to search in.
     * @param target The target to look for.
     * @return True if the place contains the target.
     */
    @CompilerDirectives.TruffleBoundary
    public static boolean contains(String place, String target) {
        return place.contains(target);
    }

    /**
     * Get the index of the target string in.
     *
     * @param in The string to search in.
     * @param target The target string.
     * @return The index of the target string.
     */
    @CompilerDirectives.TruffleBoundary
    public static int indexOf(String in, String target) {
        return in.indexOf(target);
    }

    /**
     * Split a string according to a splitter regular expression.
     *
     * @param toSplit The string to split.
     * @param splitter The splitter expression.
     * @return The split string in an array.
     */
    @CompilerDirectives.TruffleBoundary
    public static String[] split(String toSplit, String splitter) {
        return toSplit.split(splitter);
    }

    /**
     * Assuming the given string is composed of file paths separated by the OS's path separator,
     * return those paths as an array of String.
     *
     * @param toSplit The string to split.
     * @return The array of paths.
     */
    @CompilerDirectives.TruffleBoundary
    public static String[] splitPaths(String toSplit) {
        return toSplit.trim().split(File.pathSeparator);
    }

    /**
     * Get the lower case representation of the given string.
     *
     * @param toLower The string to lower.
     * @return The lowered string.
     */
    @CompilerDirectives.TruffleBoundary
    public static String toLowerCase(String toLower) {
        return toLower.toLowerCase(Locale.ROOT);
    }

    /**
     * Get the upper case representation of the given string.
     *
     * @param toUpper The string to upper.
     * @return The uppered string.
     */
    @CompilerDirectives.TruffleBoundary
    public static String toUpperCase(String toUpper) {
        return toUpper.toUpperCase(Locale.ROOT);
    }

    /**
     * Create a string representation with escaped character.
     *
     * @param source The string to represent.
     * @return The representation of the string.
     */
    @CompilerDirectives.TruffleBoundary
    public static String toRepr(String source) {
        return Utils.toRepr(source);
    }

    /**
     * Translate the escaping sequence of the given string.
     *
     * @param toTranslate The string to translate the escape sequences in.
     * @return The string with the translated escape sequence.
     */
    @CompilerDirectives.TruffleBoundary
    public static String translateEscapes(String toTranslate) {
        return toTranslate
            .replace("\\n", "\n")
            .replace("\\r", "\r")
            .replace("\\t", "\t")
            .replace("\\b", "\b")
            .replace("\\f", "\f")
            .replace("\\\"", "\"")
            .replace("\\'", "'")
            .replace("\\\\", "\\");
    }

    @CompilerDirectives.TruffleBoundary
    public static String substring(String source, long start, long end) {
        return source.substring((int) start, (int) end);
    }
}
