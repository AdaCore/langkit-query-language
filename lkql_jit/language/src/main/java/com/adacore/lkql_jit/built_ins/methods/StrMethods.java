//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.values.LKQLPattern;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleStringIterator;
import java.util.ArrayList;
import java.util.List;

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
        public TruffleString execute(
            TruffleString arg,
            @Cached TruffleString.ToJavaStringNode toJavaStringNode,
            @Cached TruffleString.FromJavaStringNode fromJavaStringNode
        ) {
            return fromJavaStringNode.execute(
                StringUtils.toLowerCase(toJavaStringNode.execute(arg)),
                Constants.STRING_ENCODING
            );
        }
    }

    @BuiltInMethod(
        name = "is_lower_case",
        doc = "Return whether the string is in lowercase",
        isProperty = true
    )
    public abstract static class IsLowerCaseExpr extends BuiltInBody {

        @Specialization
        public boolean executeGeneric(
            TruffleString arg,
            @Cached TruffleString.CreateCodePointIteratorNode createCodePointIteratorNode,
            @Cached TruffleStringIterator.NextNode nextNode
        ) {
            final var it = createCodePointIteratorNode.execute(arg, Constants.STRING_ENCODING);
            while (it.hasNext()) {
                if (!Character.isLowerCase(nextNode.execute(it))) return false;
            }
            return true;
        }
    }

    @BuiltInMethod(
        name = "to_upper_case",
        doc = "Return the string in uppercase",
        isProperty = true
    )
    public abstract static class ToUpperCaseExpr extends BuiltInBody {

        @Specialization
        public TruffleString executeGeneric(
            TruffleString arg,
            @Cached TruffleString.ToJavaStringNode toJavaStringNode,
            @Cached TruffleString.FromJavaStringNode fromJavaStringNode
        ) {
            return fromJavaStringNode.execute(
                StringUtils.toUpperCase(toJavaStringNode.execute(arg)),
                Constants.STRING_ENCODING
            );
        }
    }

    @BuiltInMethod(
        name = "is_upper_case",
        doc = "Return whether the string is in uppercase",
        isProperty = true
    )
    public abstract static class IsUpperCaseExpr extends BuiltInBody {

        @Specialization
        public boolean executeGeneric(
            TruffleString arg,
            @Cached TruffleString.CreateCodePointIteratorNode createCodePointIteratorNode,
            @Cached TruffleStringIterator.NextNode nextNode
        ) {
            final var it = createCodePointIteratorNode.execute(arg, Constants.STRING_ENCODING);
            while (it.hasNext()) {
                if (!Character.isUpperCase(nextNode.execute(it))) return false;
            }
            return true;
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
        public Object executeGeneric(
            TruffleString arg,
            @Cached TruffleString.ToJavaStringNode toJavaStringNode
        ) {
            // Get the argument as a Java string
            final var argStr = toJavaStringNode.execute(arg);

            // Prepare the result
            boolean previousUnderscore = false;
            int i = 0;

            while (i < argStr.length()) {
                // Get the character
                char c = argStr.charAt(i);

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
        public Object executeGeneric(
            TruffleString arg,
            @Cached TruffleString.CodePointLengthNode codePointLengthNode
        ) {
            // Return the length
            return (long) codePointLengthNode.execute(arg, Constants.STRING_ENCODING);
        }
    }

    @BuiltInMethod(
        name = "substring",
        doc = "Given a string and two indices (from and to), return the substring" +
        " contained between indices from and to (both included)"
    )
    abstract static class SubstringExpr extends BuiltInBody {

        @Specialization
        protected TruffleString onValid(
            TruffleString source,
            long start,
            long end,
            @Cached TruffleString.SubstringNode substringNode,
            @Cached TruffleString.CodePointLengthNode codePointLengthNode
        ) {
            // Offset the start index by 1 since LKQL is 1-indexed
            start = start - 1;
            final var length = end - start;

            // Verify start and end bounds
            if (start < 0) {
                throw LKQLRuntimeException.invalidIndex((int) start + 1, argNode(0));
            }
            if (end > codePointLengthNode.execute(source, Constants.STRING_ENCODING)) {
                throw LKQLRuntimeException.invalidIndex((int) end, argNode(1));
            }

            // Return the substring
            return substringNode.execute(
                source,
                (int) start,
                (int) length,
                Constants.STRING_ENCODING,
                false
            );
        }
    }

    @BuiltInMethod(
        name = "split",
        doc = "Given a string, split it on the given separator, and return an iterator on the" +
        " parts"
    )
    abstract static class SplitExpr extends BuiltInBody {

        @Specialization
        protected LKQLList onValid(
            TruffleString source,
            TruffleString sep,
            @Cached TruffleString.CodePointLengthNode codePointLengthNode,
            @Cached TruffleString.IndexOfStringNode indexOfStringNode,
            @Cached TruffleString.SubstringNode substringNode
        ) {
            final int sourceLen = codePointLengthNode.execute(source, Constants.STRING_ENCODING);
            final int sourceUpperBound = sourceLen + 1;
            final int sepLen = codePointLengthNode.execute(sep, Constants.STRING_ENCODING);
            int subStart = 0;
            int subLen;
            int sepIndex = 0;
            final List<TruffleString> resList = new ArrayList<>();

            while (sepIndex >= 0) {
                sepIndex = indexOfStringNode.execute(
                    source,
                    sep,
                    subStart,
                    sourceUpperBound,
                    Constants.STRING_ENCODING
                );
                if (sepIndex >= 0) {
                    subLen = sepIndex - subStart - 1;
                } else {
                    subLen = sourceLen - subStart;
                }
                resList.add(
                    substringNode.execute(
                        source,
                        subStart,
                        subLen,
                        Constants.STRING_ENCODING,
                        false
                    )
                );
                subStart = sepIndex + sepLen;
            }

            return new LKQLList(resList.toArray(new TruffleString[0]));
        }
    }

    @BuiltInMethod(
        name = "contains",
        doc = "Search for to_find in the given string. Return whether a match is" +
        " found. to_find can be either a pattern or a string"
    )
    abstract static class ContainsExpr extends BuiltInBody {

        @Specialization
        protected boolean onString(
            TruffleString source,
            TruffleString toFind,
            @Cached TruffleString.CodePointLengthNode codePointLengthNode,
            @Cached TruffleString.IndexOfStringNode indexOfStringNode
        ) {
            return (
                indexOfStringNode.execute(
                    source,
                    toFind,
                    0,
                    codePointLengthNode.execute(source, Constants.STRING_ENCODING),
                    Constants.STRING_ENCODING
                ) >=
                0
            );
        }

        @Specialization
        protected boolean onPattern(TruffleString source, LKQLPattern toFind) {
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
        protected long onString(
            TruffleString source,
            TruffleString toFind,
            @Cached TruffleString.CodePointLengthNode codePointLengthNode,
            @Cached TruffleString.IndexOfStringNode indexOfStringNode
        ) {
            final int res = indexOfStringNode.execute(
                source,
                toFind,
                0,
                codePointLengthNode.execute(source, Constants.STRING_ENCODING),
                Constants.STRING_ENCODING
            );
            return res >= 0 ? res + 1 : -1;
        }

        @Specialization
        protected long onPattern(TruffleString source, LKQLPattern toFind) {
            return toFind.find(source) + 1;
        }
    }

    @BuiltInMethod(name = "starts_with", doc = "Returns whether string starts with given prefix")
    abstract static class StartsWithExpr extends BuiltInBody {

        @Specialization
        protected boolean onValid(
            TruffleString source,
            TruffleString prefix,
            @Cached TruffleString.CodePointLengthNode codePointLengthNode,
            @Cached TruffleString.RegionEqualNode regionEqualNode
        ) {
            final var prefixLen = codePointLengthNode.execute(prefix, Constants.STRING_ENCODING);
            return regionEqualNode.execute(
                source,
                0,
                prefix,
                0,
                prefixLen,
                Constants.STRING_ENCODING
            );
        }
    }

    @BuiltInMethod(name = "ends_with", doc = "Returns whether string ends with given prefix")
    abstract static class EndsWithExpr extends BuiltInBody {

        @Specialization
        protected boolean onValid(
            TruffleString source,
            TruffleString suffix,
            @Cached TruffleString.CodePointLengthNode codePointLengthNode,
            @Cached TruffleString.RegionEqualNode regionEqualNode
        ) {
            final var suffixLen = codePointLengthNode.execute(suffix, Constants.STRING_ENCODING);
            final var sourceLen = codePointLengthNode.execute(source, Constants.STRING_ENCODING);
            return regionEqualNode.execute(
                source,
                sourceLen - suffixLen,
                suffix,
                0,
                suffixLen,
                Constants.STRING_ENCODING
            );
        }
    }
}
