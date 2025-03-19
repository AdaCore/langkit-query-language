//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.values.LKQLPattern;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.dsl.Specialization;

/**
 * This class contains all built-in methods for the string type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@BuiltinMethodContainer(targetTypes = { LKQLTypesHelper.LKQL_STRING })
public class StrMethods {

    @BuiltInMethod(
        name = "to_lower_case",
        doc = "Return the string in lowercase",
        isProperty = true
    )
    public abstract static class ToLowerCaseExpr extends BuiltInBody {

        @Specialization
        public Object onGeneric(String arg) {
            return StringUtils.toLowerCase(arg);
        }
    }

    @BuiltInMethod(
        name = "is_lower_case",
        doc = "Return whether the string is in lowercase",
        isProperty = true
    )
    public abstract static class IsLowerCaseExpr extends BuiltInBody {

        @Specialization
        public Object onGeneric(String arg) {
            return StringUtils.toLowerCase(arg).equals(arg);
        }
    }

    @BuiltInMethod(
        name = "to_upper_case",
        doc = "Return the string in uppercase",
        isProperty = true
    )
    public abstract static class ToUpperCaseExpr extends BuiltInBody {

        @Specialization
        public Object onGeneric(String arg) {
            return StringUtils.toUpperCase(arg);
        }
    }

    @BuiltInMethod(
        name = "is_upper_case",
        doc = "Return whether the string is in uppercase",
        isProperty = true
    )
    public abstract static class IsUpperCaseExpr extends BuiltInBody {

        @Specialization
        public Object onGeneric(String arg) {
            return StringUtils.toUpperCase(arg).equals(arg);
        }
    }

    @BuiltInMethod(
        name = "is_mixed_case",
        doc = "Return whether the given string is written in mixed case, that is," +
        " with only lower case characters except the first one and every" +
        " character following an underscore",
        isProperty = true
    )
    public abstract static class IsMixedCaseExpr extends BuiltInBody {

        @Specialization
        public Object onGeneric(String arg) {
            // Prepare the result
            boolean previousUnderscore = false;
            int i = 0;

            while (i < arg.length()) {
                // Get the character
                char c = arg.charAt(i);

                // Test the character
                if (i == 0 || previousUnderscore) {
                    if (Character.isLowerCase(c)) return false;
                    previousUnderscore = false;
                } else {
                    if (Character.isUpperCase(c)) return false;
                }
                if (c == '_') {
                    previousUnderscore = true;
                }

                i++;
            }

            // Return the result
            return true;
        }
    }

    @BuiltInMethod(name = "length", doc = "Return the string's length", isProperty = true)
    public abstract static class LengthExpr extends BuiltInBody {

        @Specialization
        public Object onGeneric(String arg) {
            // Return the length
            return (long) arg.length();
        }
    }

    @BuiltInMethod(
        name = "substring",
        doc = "Given a string and two indices (from and to), return the substring" +
        " contained between indices from and to (both included)"
    )
    abstract static class SubstringExpr extends BuiltInBody {

        @Specialization
        protected String onGeneric(String source, long start, long end) {
            // Offset the start index by 1 since LKQL is 1-indexed
            start = start - 1;

            // Verify start and end bounds
            if (start < 0) {
                throw LKQLRuntimeException.invalidIndex((int) start + 1, this);
            }
            if (end > source.length()) {
                throw LKQLRuntimeException.invalidIndex((int) end, this);
            }

            // Return the substring
            return source.substring((int) start, (int) end);
        }
    }

    @BuiltInMethod(
        name = "split",
        doc = "Given a string, split it on the given separator, and return an iterator on the" +
        " parts"
    )
    abstract static class SplitExpr extends BuiltInBody {

        @Specialization
        protected LKQLList onGeneric(String source, String sep) {
            return new LKQLList(StringUtils.split(source, sep));
        }
    }

    @BuiltInMethod(
        name = "contains",
        doc = "Search for to_find in the given string. Return whether a match is" +
        " found. to_find can be either a pattern or a string"
    )
    abstract static class ContainsExpr extends BuiltInBody {

        @Specialization
        protected boolean onString(String source, String toFind) {
            return StringUtils.contains(source, toFind);
        }

        @Specialization
        protected boolean onPattern(String source, LKQLPattern toFind) {
            return toFind.contains(source);
        }
    }

    @BuiltInMethod(
        name = "find",
        doc = "Search for to_find in the given string. Return position of the match," +
        " or -1 if no match. to_find can be either a pattern or a string"
    )
    abstract static class FindExpr extends BuiltInBody {

        @Specialization
        protected long onString(String source, String toFind) {
            return StringUtils.indexOf(source, toFind) + 1;
        }

        @Specialization
        protected long onPattern(String source, LKQLPattern toFind) {
            return toFind.find(source) + 1;
        }
    }

    @BuiltInMethod(name = "starts_with", doc = "Returns whether string starts with given prefix")
    abstract static class StartsWithExpr extends BuiltInBody {

        @Specialization
        protected boolean onGeneric(String source, String prefix) {
            return source.startsWith(prefix);
        }
    }

    @BuiltInMethod(name = "ends_with", doc = "Returns whether string ends with given prefix")
    abstract static class EndsWithExpr extends BuiltInBody {

        @Specialization
        protected boolean onGeneric(String source, String suffix) {
            return source.endsWith(suffix);
        }
    }
}
