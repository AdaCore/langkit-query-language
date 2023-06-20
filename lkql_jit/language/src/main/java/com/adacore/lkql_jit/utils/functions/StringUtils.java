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

package com.adacore.lkql_jit.utils.functions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.oracle.truffle.api.CompilerDirectives;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.function.Consumer;

/**
 * Util functions to manipulate the java string type in the JIT implementation.
 *
 * @author Hugo GUERRIER
 */
public final class StringUtils {

    // ----- Macros -----

    // --- The color constants
    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_RED = "\u001B[31m";
    public static final String ANSI_BLUE = "\u001B[34m";
    public static final String ANSI_YELLOW = "\u001B[33m";
    public static final String ANSI_GREY = "\u001B[38;5;246m";
    public static final String ANSI_BOLD = "\u001B[1m";

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
     * Compare the given strings ignoring the casing of them.
     *
     * @param left  The left string.
     * @param right The right string.
     * @return True if the strings are equals, false else.
     */
    @CompilerDirectives.TruffleBoundary
    public static boolean equalsIgnoreCase(String left, String right) {
        return left.equalsIgnoreCase(right);
    }

    /**
     * Fill the given string with space character to get to the given size.
     *
     * @param toFill The string to fill with space.
     * @param size   The size to get to.
     * @return The filled string.
     */
    @CompilerDirectives.TruffleBoundary
    public static String fill(String toFill, int size) {
        int missing = size - toFill.length();
        return toFill + " ".repeat(Math.max(0, missing));
    }

    /**
     * Get if a string contains a given target.
     *
     * @param place  The place to search in.
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
     * @param in     The string to search in.
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
     * @param toSplit  The string to split.
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
     * Translate a string to a snake_case one.
     *
     * @param source The source string.
     * @return The transformed string.
     */
    @CompilerDirectives.TruffleBoundary
    public static String toSnakeCase(String source) {
        StringBuilder res = new StringBuilder();
        for (int i = 0; i < source.length(); i++) {
            char curChar = source.charAt(i);
            if (Character.isUpperCase(curChar)) {
                res.append('_');
                curChar = Character.toLowerCase(curChar);
            }
            res.append(curChar);
        }
        return res.toString();
    }

    /**
     * Get a camel case formatted string from a snake case source.
     *
     * @param source The source string.
     * @return The camel case.
     */
    @CompilerDirectives.TruffleBoundary
    public static String toCamelCase(String source) {
        StringBuilder res = new StringBuilder();
        int i = 0;
        while (i < source.length()) {
            char curChar = source.charAt(i);
            if (curChar == '_') {
                i++;
                curChar = Character.toUpperCase(source.charAt(i));
            }
            res.append(curChar);
            i++;
        }
        return res.toString();
    }

    /**
     * Create a string representation with escaped character.
     *
     * @param source The string to represent.
     * @return The representation of the string.
     */
    @CompilerDirectives.TruffleBoundary
    public static String toRepr(String source) {
        return "\"" +
            source
                .replace("\"", "\\\"")
                .replace("\n", "\\x0a") +
            "\"";
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

    /**
     * Split the source by lines.
     *
     * @param source The source to split.
     * @return The lines in an array.
     */
    @CompilerDirectives.TruffleBoundary
    public static String[] separateLines(String source) {
        // Prepare the result and the working variables
        List<String> lines = new ArrayList<>();
        StringBuilder buffer = new StringBuilder();

        // Iterate over the character to separate the lines
        for (int i = 0; i < source.length(); i++) {
            char c = source.charAt(i);
            if (c == '\n') {
                lines.add(buffer.toString());
                buffer.delete(0, buffer.length());
            } else {
                buffer.append(c);
            }
        }
        lines.add(buffer.toString());

        // Return the separated lines
        return lines.toArray(new String[0]);
    }

    /**
     * Get the underlined source representation.
     *
     * @param lines          The lines to display.
     * @param startLine      The starting line.
     * @param startCol       The starting column.
     * @param endLine        The ending line.
     * @param endCol         The ending colum.
     * @param underLineColor The color of the underline.
     * @return The underlined lines in a string.
     */
    @CompilerDirectives.TruffleBoundary
    public static String underlineSource(
        String[] lines,
        int startLine,
        int startCol,
        int endLine,
        int endCol,
        String underLineColor
    ) {
        // Prepare the result
        StringBuilder res = new StringBuilder();
        int colSize = String.valueOf(endLine).length();

        // Create the function to start a line
        Consumer<Integer> lineStarting = (lineNum) -> {
            res.append(LKQLLanguage.SUPPORT_COLOR ? ANSI_BLUE : "");
            if (lineNum < 1) {
                res.append(" ".repeat(colSize));
            } else {
                res.append(fill(String.valueOf(lineNum), colSize));
            }
            res.append(" |")
                .append(LKQLLanguage.SUPPORT_COLOR ? ANSI_RESET : "");
        };

        // If the source is single line
        if (lines.length == 1) {
            lineStarting.accept(startLine);
            res.append(' ')
                .append(lines[0]);
            if (startCol != endCol) {
                res.append('\n');
                lineStarting.accept(0);
                res.append(LKQLLanguage.SUPPORT_COLOR ? underLineColor : "")
                    .append(" ".repeat(startCol))
                    .append("^".repeat(Math.max(0, endCol - startCol)));
            }
        }

        // Else do the multiline display
        else {
            int difference = endLine - startLine - 1;
            lineStarting.accept(startLine);
            res.append("  ")
                .append(lines[0])
                .append("\n");
            lineStarting.accept(0);
            res.append(LKQLLanguage.SUPPORT_COLOR ? underLineColor : "")
                .append(" ")
                .append("_".repeat(startCol))
                .append("^\n");

            if (difference > 0) {
                lineStarting.accept(0);
                res.append(LKQLLanguage.SUPPORT_COLOR ? underLineColor : "")
                    .append("|\n");
                lineStarting.accept(0);
                res.append(LKQLLanguage.SUPPORT_COLOR ? underLineColor : "")
                    .append('|')
                    .append(" ~~~ ")
                    .append(difference)
                    .append(" other lines ~~~\n");
                lineStarting.accept(0);
                res.append(LKQLLanguage.SUPPORT_COLOR ? underLineColor : "")
                    .append("|\n");
            }

            lineStarting.accept(endLine);
            res.append(LKQLLanguage.SUPPORT_COLOR ? underLineColor : "")
                .append("| ")
                .append(LKQLLanguage.SUPPORT_COLOR ? ANSI_RESET : "")
                .append(lines[lines.length - 1])
                .append('\n');
            lineStarting.accept(0);
            res.append(LKQLLanguage.SUPPORT_COLOR ? underLineColor : "")
                .append("|")
                .append("_".repeat(Math.max(1, endCol - 1)))
                .append("^");
        }

        // Return the underlined sources
        res.append(LKQLLanguage.SUPPORT_COLOR ? ANSI_RESET : "");
        return res.toString();
    }

}
