//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInFunctionValue.create;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.BuiltinFunctionBody;
import com.adacore.lkql_jit.built_ins.functions.BaseNameFunction;
import com.adacore.lkql_jit.built_ins.values.LKQLPattern;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLList;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.BigIntegerUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.math.BigInteger;
import java.util.Map;

/**
 * This class contains all built-in methods for the string type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public class StrMethods {

    public static final Map<String, BuiltInFunctionValue> methods =
            Map.ofEntries(
                    Map.entry(BaseNameFunction.NAME, BaseNameFunction.getValue()),
                    create(
                            "to_lower_case",
                            "Return the given string written with lower case characters only",
                            new String[] {"str"},
                            new Expr[] {null},
                            new ToLowerCaseExpr()),
                    create(
                            "is_lower_case",
                            "Return whether the given string contains lower case characters only",
                            new String[] {"str"},
                            new Expr[] {null},
                            new IsLowerCaseExpr()),
                    create(
                            "to_upper_case",
                            "Return the given string written with upper case characters only",
                            new String[] {"str"},
                            new Expr[] {null},
                            new ToUpperCaseExpr()),
                    create(
                            "is_upper_case",
                            "Return whether the given string contains upper case characters only",
                            new String[] {"str"},
                            new Expr[] {null},
                            new IsUpperCaseExpr()),
                    create(
                            "is_mixed_case",
                            "Return whether the given string is written in mixed case, that is,"
                                + " with only lower case characters except the first one and every"
                                + " character following an underscore",
                            new String[] {"str"},
                            new Expr[] {null},
                            new IsMixedCaseExpr()),
                    create(
                            "length",
                            "Given a string, return the length of it in character",
                            new String[] {"str"},
                            new Expr[] {null},
                            new LengthExpr()),
                    create(
                            "substring",
                            "Given a string and two indices (from and to), return the substring"
                                    + " contained between indices from and to (both included)",
                            new String[] {"str", "from", "to"},
                            new Expr[] {null, null, null},
                            new SubstringExpr()),
                    create(
                            "split",
                            "Given a string, return an iterator on the words contained by str"
                                    + " separated by separator",
                            new String[] {"str", "separator"},
                            new Expr[] {null, null},
                            new SplitExpr()),
                    create(
                            "contains",
                            "Search for to_find in the given string. Return whether a match is"
                                    + " found. to_find can be either a pattern or a string",
                            new String[] {"str", "to_find"},
                            new Expr[] {null, null},
                            new ContainsExpr()),
                    create(
                            "find",
                            "Search for to_find in the given string. Return position of the match,"
                                + " or -1 if no match. to_find can be either a pattern or a string",
                            new String[] {"str", "to_find"},
                            new Expr[] {null, null},
                            new FindExpr()),
                    create(
                            "starts_with",
                            "Given a string, returns whether it starts with the given prefix",
                            new String[] {"str", "prefix"},
                            new Expr[] {null, null},
                            new StartsWithExpr()),
                    create(
                            "ends_with",
                            "Given a string, returns whether it ends with the given suffix",
                            new String[] {"str", "suffix"},
                            new Expr[] {null, null},
                            new EndsWithExpr()));

    // ----- Inner classes -----

    /** Expression of the "to_lower_case" method. */
    public static final class ToLowerCaseExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return StringUtils.toLowerCase(LKQLTypeSystemGen.asString(frame.getArguments()[0]));
        }
    }

    /** Expression of the "is_lower_case" method. */
    public static final class IsLowerCaseExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            String arg = LKQLTypeSystemGen.asString(frame.getArguments()[0]);
            return StringUtils.toLowerCase(arg).equals(arg);
        }
    }

    /** Expression of the "to_upper_case" method. */
    public static final class ToUpperCaseExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return StringUtils.toUpperCase(LKQLTypeSystemGen.asString(frame.getArguments()[0]));
        }
    }

    /** Expression of the "is_upper_case" method. */
    public static final class IsUpperCaseExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            String arg = LKQLTypeSystemGen.asString(frame.getArguments()[0]);
            return StringUtils.toUpperCase(arg).equals(arg);
        }
    }

    /** Expression of the "is_mixed_case" method. */
    public static final class IsMixedCaseExpr extends BuiltinFunctionBody {
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
    public static final class LengthExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the receiver
            String receiver = LKQLTypeSystemGen.asString(frame.getArguments()[0]);

            // Return the length
            return (long) receiver.length();
        }
    }

    /** Expression of the "substring" method. */
    public static final class SubstringExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            Object startObject = frame.getArguments()[1];
            Object endObject = frame.getArguments()[2];

            // Verify the type of arguments
            if (!LKQLTypeSystemGen.isImplicitBigInteger(startObject)) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_INTEGER,
                        LKQLTypesHelper.fromJava(startObject),
                        this.callNode.getArgList().getArgs()[0]);
            }

            if (!LKQLTypeSystemGen.isImplicitBigInteger(endObject)) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_INTEGER,
                        LKQLTypesHelper.fromJava(endObject),
                        this.callNode.getArgList().getArgs()[1]);
            }

            // Cast the arguments
            BigInteger startBig =
                    BigIntegerUtils.subtract(
                            LKQLTypeSystemGen.asImplicitBigInteger(startObject), BigInteger.ONE);
            BigInteger endBig = LKQLTypeSystemGen.asImplicitBigInteger(endObject);

            int start = BigIntegerUtils.intValue(startBig);
            int end = BigIntegerUtils.intValue(endBig);

            // Verify the start and end
            if (start < 0) {
                throw LKQLRuntimeException.invalidIndex(
                        start, this.callNode.getArgList().getArgs()[0]);
            }
            if (end > LKQLTypeSystemGen.asString(frame.getArguments()[0]).length()) {
                throw LKQLRuntimeException.invalidIndex(
                        end, this.callNode.getArgList().getArgs()[1]);
            }

            // Return the substring
            return LKQLTypeSystemGen.asString(frame.getArguments()[0]).substring(start, end);
        }
    }

    /** Expression of the "split" method. */
    public static final class SplitExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the argument
            Object toSplit = frame.getArguments()[0];
            Object separatorObject = frame.getArguments()[1];

            // Verify the argument type
            if (!LKQLTypeSystemGen.isString(separatorObject)) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_STRING,
                        LKQLTypesHelper.fromJava(separatorObject),
                        this.callNode.getArgList().getArgs()[0]);
            }

            // Split the string
            String[] separated =
                    StringUtils.split(
                            LKQLTypeSystemGen.asString(toSplit),
                            LKQLTypeSystemGen.asString(separatorObject));

            // Return the list value of the split string
            return new LKQLList(separated);
        }
    }

    /** Expression of the "contains" method. */
    public static final class ContainsExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            String receiver = LKQLTypeSystemGen.asString(frame.getArguments()[0]);
            Object toFindObject = frame.getArguments()[1];
            boolean contains;

            // If the argument is a string
            if (LKQLTypeSystemGen.isString(toFindObject)) {
                String toFind = LKQLTypeSystemGen.asString(toFindObject);
                contains = StringUtils.contains(receiver, toFind);
            }

            // If the argument is a pattern
            else if (LKQLTypeSystemGen.isLKQLPattern(toFindObject)) {
                LKQLPattern pattern = LKQLTypeSystemGen.asLKQLPattern(toFindObject);
                contains = pattern.contains(receiver);
            }

            // Else, just thrown an error
            else {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_STRING,
                        LKQLTypesHelper.fromJava(toFindObject),
                        this.callNode.getArgList().getArgs()[0]);
            }

            // Return if the receiver contains the to find
            return contains;
        }
    }

    /** Expression of the "find" method. */
    public static final class FindExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            String receiver = LKQLTypeSystemGen.asString(frame.getArguments()[0]);
            Object toFindObject = frame.getArguments()[1];
            int index;

            // If the argument is a string
            if (LKQLTypeSystemGen.isString(toFindObject)) {
                String toFind = LKQLTypeSystemGen.asString(toFindObject);
                index = StringUtils.indexOf(receiver, toFind);
            }

            // If the argument is a pattern
            else if (LKQLTypeSystemGen.isLKQLPattern(toFindObject)) {
                LKQLPattern pattern = LKQLTypeSystemGen.asLKQLPattern(toFindObject);
                index = pattern.find(receiver);
            }

            // Else, just throw an error
            else {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_STRING,
                        LKQLTypesHelper.fromJava(toFindObject),
                        this.callNode.getArgList().getArgs()[0]);
            }

            // Return the index
            return (long) index + 1;
        }
    }

    /** Expression of the "starts_with" method. */
    public static final class StartsWithExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the argument
            Object prefixObject = frame.getArguments()[1];

            // Verify the argument type
            if (!LKQLTypeSystemGen.isString(prefixObject)) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_STRING,
                        LKQLTypesHelper.fromJava(prefixObject),
                        this.callNode.getArgList().getArgs()[0]);
            }

            // Cast the arguments
            String receiver = LKQLTypeSystemGen.asString(frame.getArguments()[0]);
            String prefix = LKQLTypeSystemGen.asString(prefixObject);

            // Return if the receiver has the prefix
            return receiver.startsWith(prefix);
        }
    }

    /** Expression of the "ends_with" method. */
    public static final class EndsWithExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the argument
            Object suffixObject = frame.getArguments()[1];

            // Verify the argument type
            if (!LKQLTypeSystemGen.isString(suffixObject)) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_STRING,
                        LKQLTypesHelper.fromJava(suffixObject),
                        this.callNode.getArgList().getArgs()[0]);
            }

            // Cast the arguments
            String receiver = LKQLTypeSystemGen.asString(frame.getArguments()[0]);
            String suffix = LKQLTypeSystemGen.asString(suffixObject);

            // Return if the receiver has the prefix
            return receiver.endsWith(suffix);
        }
    }
}
