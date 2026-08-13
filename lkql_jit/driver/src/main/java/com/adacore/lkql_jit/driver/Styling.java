//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver;

import java.util.function.Function;

/** Utility class that hold tools to output styled texts. */
public final class Styling {

    // ----- Styling functions -----

    public static String bold(String s) {
        return styled(s, "\u001B[1m");
    }

    public static String underline(String s) {
        return styled(s, "\u001B[4m");
    }

    public static String brightBlack(String s) {
        return styled(s, "\u001B[90m");
    }

    public static String red(String s) {
        return styled(s, "\u001B[31m");
    }

    public static String green(String s) {
        return styled(s, "\u001B[32m");
    }

    public static String brightGreen(String s) {
        return styled(s, "\u001B[92m");
    }

    public static String blue(String s) {
        return styled(s, "\u001B[34m");
    }

    public static String brightBlue(String s) {
        return styled(s, "\u001B[94m");
    }

    public static String yellow(String s) {
        return styled(s, "\u001B[33m");
    }

    // ----- Internal helpers -----

    private static String styled(String s, String ansiStyle) {
        return ansiStyle + s + "\u001B[0m";
    }

    // ----- Inner classes -----

    /**
     * This interface defines a function that can be called to color a text with an ANSI sequence.
     */
    public interface StylingFunction extends Function<String, String> {}
}
