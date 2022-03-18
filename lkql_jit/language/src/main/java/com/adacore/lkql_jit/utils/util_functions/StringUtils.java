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

import com.adacore.lkql_jit.LKQLLanguage;
import com.oracle.truffle.api.CompilerDirectives;

import java.util.Locale;

/**
 * Util functions to manipulate the java string type in the JIT implementation
 *
 * @author Hugo GUERRIER
 */
public final class StringUtils {

    // ----- Macros -----

    // --- The color constants
    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_RED = "\u001B[31;1m";
    public static final String ANSI_BLUE = "\u001B[34;1m";
    public static final String ANSI_YELLOW = "\u001B[33;1m";
    public static final String ANSI_BOLD = "\u001B[1m";

    // ----- Class methods -----

    /**
     * Concatenate given strings
     *
     * @param strings The strings to concatenate
     * @return The concatenated strings
     */
    @CompilerDirectives.TruffleBoundary
    public static String concat(String ...strings) {
        StringBuilder builder = new StringBuilder();
        for(String s : strings) {
            builder.append(s);
        }
        return builder.toString();
    }

    /**
     * Get if a string contains a given target
     *
     * @param place The place to search in
     * @param target The target to look for
     * @return True if the place contains the target
     */
    @CompilerDirectives.TruffleBoundary
    public static boolean contains(String place, String target) {
        return place.contains(target);
    }

    /**
     * Split a string according to a splitter regular expression
     *
     * @param toSplit The string to split
     * @param splitter The splitter expression
     * @return The split string in an array
     */
    @CompilerDirectives.TruffleBoundary
    public static String[] split(String toSplit, String splitter) {
        return toSplit.split(splitter);
    }

    /**
     * Get the lower case representation of the given string
     *
     * @param toLower The string to lower
     * @return The lowered string
     */
    @CompilerDirectives.TruffleBoundary
    public static String toLowerCase(String toLower) {
        return toLower.toLowerCase(Locale.ROOT);
    }

    /**
     * Get the upper case representation of the given string
     *
     * @param toUpper The string to upper
     * @return The uppered string
     */
    @CompilerDirectives.TruffleBoundary
    public static String toUpperCase(String toUpper) {
        return toUpper.toUpperCase(Locale.ROOT);
    }

    /**
     * Translate a string to a snake_case one
     *
     * @param source The source string
     * @return The transformed string
     */
    @CompilerDirectives.TruffleBoundary
    public static String toSnakeCase(String source) {
        StringBuilder res = new StringBuilder();
        for(int i = 0 ; i < source.length() ; i++) {
            char curChar = source.charAt(i);
            if(Character.isUpperCase(curChar)) {
                res.append('_');
                curChar = Character.toLowerCase(curChar);
            }
            res.append(curChar);
        }
        return res.toString();
    }

    /**
     * Get a camel case formatted string from a snake case source
     *
     * @param source The source string
     * @return The camel case
     */
    @CompilerDirectives.TruffleBoundary
    public static String toCamelCase(String source) {
        StringBuilder res = new StringBuilder();
        int i = 0;
        while(i < source.length()) {
            char curChar = source.charAt(i);
            if(curChar == '_') {
                i++;
                curChar = Character.toUpperCase(source.charAt(i));
            }
            res.append(curChar);
            i++;
        }
        return res.toString();
    }

    /**
     * Create a string representation with escaped character
     *
     * @param source The string to represent
     * @return The representation of the string
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
     * Get the underlined source representation from the given params
     *
     * @param lines The lines to display
     * @param startCol The starting colum
     * @param endCol The ending column
     * @param underLineColor The color of the underline
     * @return The underlined lines in a string
     */
    @CompilerDirectives.TruffleBoundary
    public static String underlinedSource(
            String[] lines,
            int startLine,
            int endLine,
            int startCol,
            int endCol,
            String underLineColor
    ) {
        // Prepare the result string builder
        StringBuilder res = new StringBuilder();

        // Iterate on lines
        for(int i = startLine ; i <= endLine ; i++) {
            String line = lines[i - startLine];

            res.append(LKQLLanguage.SUPPORT_COLOR ? ANSI_BLUE : "")
                    .append(i)
                    .append(" | ")
                    .append(LKQLLanguage.SUPPORT_COLOR ? ANSI_RESET : "")
                    .append(line)
                    .append('\n');

            if(startCol <= line.length()) {
                res.append(LKQLLanguage.SUPPORT_COLOR ? StringUtils.ANSI_BLUE : "")
                        .append(" ".repeat(String.valueOf(i).length()))
                        .append(" | ")
                        .append(LKQLLanguage.SUPPORT_COLOR ? underLineColor : "");
                for(int j = 0 ; j < line.length() ; j++) {
                    char toAdd = '^';
                    if(i == startLine && j + 1 < startCol) toAdd = ' ';
                    if(i == endLine && j + 1 >= endCol) break;
                    res.append(toAdd);
                }
                res.append(LKQLLanguage.SUPPORT_COLOR ? ANSI_RESET : "")
                        .append('\n');
            }
        }

        // Return the result
        return res.toString();
    }

}
