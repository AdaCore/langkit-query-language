//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.adacore.lkql_jit.values.interop.Utils;
import com.oracle.truffle.api.CompilerDirectives;
import java.io.File;
import java.util.Locale;

/** Util functions to manipulate the Java string type with Truffle boundaries. */
public final class StringUtils {

    // ----- Class methods -----

    /** Concatenate given strings. */
    @CompilerDirectives.TruffleBoundary
    public static String concat(String... strings) {
        StringBuilder builder = new StringBuilder();
        for (String s : strings) {
            builder.append(s);
        }
        return builder.toString();
    }

    /** Return whether a string contains a given sub-string. */
    @CompilerDirectives.TruffleBoundary
    public static boolean contains(String haystack, String substring) {
        return haystack.contains(substring);
    }

    /** Get the index of the first occurrence of the sub-string in another string. */
    @CompilerDirectives.TruffleBoundary
    public static int indexOf(String haystack, String substring) {
        return haystack.indexOf(substring);
    }

    /** Split a string according to the provided regular expression. */
    @CompilerDirectives.TruffleBoundary
    public static String[] split(String toSplit, String splitter) {
        return toSplit.split(splitter);
    }

    /**
     * Assuming the given string is composed of file paths separated by the OS's path separator,
     * return those paths as an array of strings.
     */
    @CompilerDirectives.TruffleBoundary
    public static String[] splitPaths(String toSplit) {
        return toSplit.trim().split(File.pathSeparator);
    }

    /** Get the lower case transformation of the given string. */
    @CompilerDirectives.TruffleBoundary
    public static String toLowerCase(String toLower) {
        return toLower.toLowerCase(Locale.ROOT);
    }

    /** Get the upper case transformation of the given string. */
    @CompilerDirectives.TruffleBoundary
    public static String toUpperCase(String toUpper) {
        return toUpper.toUpperCase(Locale.ROOT);
    }

    /** Get the literal representation of the provided string. */
    @CompilerDirectives.TruffleBoundary
    public static String toRepr(String source) {
        return Utils.toRepr(source);
    }

    /** Translate all escaping sequences in the given string. */
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

    /** Get a substring from the provided one (start inclusive, end exclusive). */
    @CompilerDirectives.TruffleBoundary
    public static String substring(String source, long start, long end) {
        return source.substring((int) start, (int) end);
    }
}
