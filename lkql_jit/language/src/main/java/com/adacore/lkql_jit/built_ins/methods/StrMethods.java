//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.createAttribute;
import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.createMethod;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.AbstractBuiltInFunctionBody;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.adacore.lkql_jit.built_ins.SpecializedBuiltInBody;
import com.adacore.lkql_jit.built_ins.functions.BaseNameFunction;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLPattern;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.util.Map;

/**
 * This class contains all built-in methods for the string type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public class StrMethods {

    public static final Map<String, BuiltInMethodFactory> methods =
            Map.ofEntries(
                    Map.entry(
                            BaseNameFunction.NAME,
                            BuiltInMethodFactory.fromFunctionValue(
                                    BaseNameFunction.getValue(), true)),
                    createAttribute(
                            "to_lower_case",
                            "Return the given string written with lower case characters only",
                            new ToLowerCaseExpr()),
                    createAttribute(
                            "is_lower_case",
                            "Return whether the given string contains lower case characters only",
                            new IsLowerCaseExpr()),
                    createAttribute(
                            "to_upper_case",
                            "Return the given string written with upper case characters only",
                            new ToUpperCaseExpr()),
                    createAttribute(
                            "is_upper_case",
                            "Return whether the given string contains upper case characters only",
                            new IsUpperCaseExpr()),
                    createAttribute(
                            "is_mixed_case",
                            "Return whether the given string is written in mixed case, that is,"
                                + " with only lower case characters except the first one and every"
                                + " character following an underscore",
                            new IsMixedCaseExpr()),
                    createAttribute(
                            "length",
                            "Given a string, return the length of it in character",
                            new LengthExpr()),
                    createMethod(
                            "substring",
                            "Given a string and two indices (from and to), return the substring"
                                    + " contained between indices from and to (both included)",
                            new String[] {"from", "to"},
                            new Expr[] {null, null},
                            new SpecializedBuiltInBody<>(
                                    StrMethodsFactory.SubstringExprNodeGen.create()) {
                                @Override
                                protected Object dispatch(Object[] args) {
                                    return this.specializedNode.executeSubstring(
                                            LKQLTypeSystemGen.asString(args[0]), args[1], args[2]);
                                }
                            }),
                    createMethod(
                            "split",
                            "Given a string, return an iterator on the words contained by str"
                                    + " separated by separator",
                            new String[] {"separator"},
                            new Expr[] {null},
                            new SpecializedBuiltInBody<>(
                                    StrMethodsFactory.SplitExprNodeGen.create()) {
                                @Override
                                protected Object dispatch(Object[] args) {
                                    return this.specializedNode.executeSplit(
                                            LKQLTypeSystemGen.asString(args[0]), args[1]);
                                }
                            }),
                    createMethod(
                            "contains",
                            "Search for to_find in the given string. Return whether a match is"
                                    + " found. to_find can be either a pattern or a string",
                            new String[] {"to_find"},
                            new Expr[] {null},
                            new SpecializedBuiltInBody<>(
                                    StrMethodsFactory.ContainsExprNodeGen.create()) {
                                @Override
                                protected Object dispatch(Object[] args) {
                                    return this.specializedNode.executeContains(
                                            LKQLTypeSystemGen.asString(args[0]), args[1]);
                                }
                            }),
                    createMethod(
                            "find",
                            "Search for to_find in the given string. Return position of the match,"
                                + " or -1 if no match. to_find can be either a pattern or a string",
                            new String[] {"to_find"},
                            new Expr[] {null},
                            new SpecializedBuiltInBody<>(
                                    StrMethodsFactory.FindExprNodeGen.create()) {
                                @Override
                                protected Object dispatch(Object[] args) {
                                    return this.specializedNode.executeFind(
                                            LKQLTypeSystemGen.asString(args[0]), args[1]);
                                }
                            }),
                    createMethod(
                            "starts_with",
                            "Given a string, returns whether it starts with the given prefix",
                            new String[] {"prefix"},
                            new Expr[] {null},
                            new SpecializedBuiltInBody<>(
                                    StrMethodsFactory.StartsWithExprNodeGen.create()) {
                                @Override
                                protected Object dispatch(Object[] args) {
                                    return this.specializedNode.executeStartsWith(
                                            LKQLTypeSystemGen.asString(args[0]), args[1]);
                                }
                            }),
                    createMethod(
                            "ends_with",
                            "Given a string, returns whether it ends with the given suffix",
                            new String[] {"suffix"},
                            new Expr[] {null},
                            new SpecializedBuiltInBody<>(
                                    StrMethodsFactory.EndsWithExprNodeGen.create()) {
                                @Override
                                protected Object dispatch(Object[] args) {
                                    return this.specializedNode.executeEndsWith(
                                            LKQLTypeSystemGen.asString(args[0]), args[1]);
                                }
                            }));

    // ----- Inner classes -----

    /** Expression of the "to_lower_case" method. */
    public static final class ToLowerCaseExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return StringUtils.toLowerCase(LKQLTypeSystemGen.asString(frame.getArguments()[0]));
        }
    }

    /** Expression of the "is_lower_case" method. */
    public static final class IsLowerCaseExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            String arg = LKQLTypeSystemGen.asString(frame.getArguments()[0]);
            return StringUtils.toLowerCase(arg).equals(arg);
        }
    }

    /** Expression of the "to_upper_case" method. */
    public static final class ToUpperCaseExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return StringUtils.toUpperCase(LKQLTypeSystemGen.asString(frame.getArguments()[0]));
        }
    }

    /** Expression of the "is_upper_case" method. */
    public static final class IsUpperCaseExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            String arg = LKQLTypeSystemGen.asString(frame.getArguments()[0]);
            return StringUtils.toUpperCase(arg).equals(arg);
        }
    }

    /** Expression of the "is_mixed_case" method. */
    public static final class IsMixedCaseExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the receiver
            String receiver = LKQLTypeSystemGen.asString(frame.getArguments()[0]);

            // Prepare the result
            boolean previousUnderscore = false;
            int i = 0;

            while (i < receiver.length()) {
                // Get the character
                char c = receiver.charAt(i);

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

    /** Expression of the "length" method. */
    public static final class LengthExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the receiver
            String receiver = LKQLTypeSystemGen.asString(frame.getArguments()[0]);

            // Return the length
            return (long) receiver.length();
        }
    }

    /** Expression of the "substring" method. */
    abstract static class SubstringExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract String executeSubstring(String source, Object start, Object end);

        @Specialization
        protected String onValid(String source, long start, long end) {
            // Offset the start index by 1 since LKQL is 1-indexed
            start = start - 1;

            // Verify start and end bounds
            if (start < 0) {
                throw LKQLRuntimeException.invalidIndex((int) start + 1, this.body.argNode(0));
            }
            if (end > source.length()) {
                throw LKQLRuntimeException.invalidIndex((int) end, this.body.argNode(1));
            }

            // Return the substring
            return source.substring((int) start, (int) end);
        }

        @Specialization
        protected String invalidEnd(
                @SuppressWarnings("unused") String source,
                @SuppressWarnings("unused") long start,
                Object end) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_INTEGER,
                    LKQLTypesHelper.fromJava(end),
                    this.body.argNode(1));
        }

        @Fallback
        protected String invalidStart(
                @SuppressWarnings("unused") String source,
                Object start,
                @SuppressWarnings("unused") Object end) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_INTEGER,
                    LKQLTypesHelper.fromJava(start),
                    this.body.argNode(0));
        }
    }

    /** Expression of the "split" method. */
    abstract static class SplitExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract LKQLList executeSplit(String source, Object sep);

        @Specialization
        protected LKQLList onValid(String source, String sep) {
            return new LKQLList(StringUtils.split(source, sep));
        }

        @Fallback
        protected LKQLList onInvalid(@SuppressWarnings("unused") String source, Object sep) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_STRING,
                    LKQLTypesHelper.fromJava(sep),
                    this.body.argNode(0));
        }
    }

    /** Expression of the "contains" method. */
    abstract static class ContainsExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract boolean executeContains(String source, Object toFind);

        @Specialization
        protected boolean onString(String source, String toFind) {
            return StringUtils.contains(source, toFind);
        }

        @Specialization
        protected boolean onPattern(String source, LKQLPattern toFind) {
            return toFind.contains(source);
        }

        @Fallback
        protected boolean onInvalid(@SuppressWarnings("unused") String source, Object toFind) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.typeUnion(
                            LKQLTypesHelper.LKQL_STRING, LKQLTypesHelper.LKQL_PATTERN),
                    LKQLTypesHelper.fromJava(toFind),
                    this.body.argNode(0));
        }
    }

    /** Expression of the "find" method. */
    abstract static class FindExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract long executeFind(String source, Object toFind);

        @Specialization
        protected long onString(String source, String toFind) {
            return StringUtils.indexOf(source, toFind) + 1;
        }

        @Specialization
        protected long onPattern(String source, LKQLPattern toFind) {
            return toFind.find(source) + 1;
        }

        @Fallback
        protected long onInvalid(@SuppressWarnings("unused") String source, Object toFind) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.typeUnion(
                            LKQLTypesHelper.LKQL_STRING, LKQLTypesHelper.LKQL_PATTERN),
                    LKQLTypesHelper.fromJava(toFind),
                    this.body.argNode(0));
        }
    }

    /** Expression of the "starts_with" method. */
    abstract static class StartsWithExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract boolean executeStartsWith(String source, Object prefix);

        @Specialization
        protected boolean onValid(String source, String prefix) {
            return source.startsWith(prefix);
        }

        @Fallback
        protected boolean onInvalid(@SuppressWarnings("unused") String source, Object prefix) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_STRING,
                    LKQLTypesHelper.fromJava(prefix),
                    this.body.argNode(0));
        }
    }

    /** Expression of the "ends_with" method. */
    abstract static class EndsWithExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract boolean executeEndsWith(String source, Object suffix);

        @Specialization
        protected boolean onValid(String source, String suffix) {
            return source.endsWith(suffix);
        }

        @Specialization
        protected boolean onInvalid(@SuppressWarnings("unused") String source, Object suffix) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_STRING,
                    LKQLTypesHelper.fromJava(suffix),
                    this.body.argNode(0));
        }
    }
}
