//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.createAttribute;
import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.createMethod;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.AbstractBuiltInFunctionBody;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.literals.BooleanLiteral;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import java.util.Map;

/**
 * This class contains all built-in methods for the token type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class TokenMethods {

    public static final Map<String, BuiltInMethodFactory> methods =
            Map.ofEntries(
                    createAttribute("start_column", "Return the column start", new StartColExpr()),
                    createAttribute("end_column", "Return the column end", new EndColExpr()),
                    createAttribute("start_line", "Return the line start", new StartLineExpr()),
                    createAttribute("end_line", "Return the line end", new EndLineExpr()),
                    createMethod(
                            "is_equivalent",
                            "Return whether two tokens are structurally equivalent",
                            new String[] {"other"},
                            new Expr[] {null},
                            new IsEquivalentExpr()),
                    createAttribute(
                            "is_trivia",
                            "Return whether this token is a trivia",
                            new IsTriviaExpr()),
                    createMethod(
                            "next",
                            "Return the next token",
                            new String[] {"exclude_trivia"},
                            new Expr[] {new BooleanLiteral(null, false)},
                            new NextExpr()),
                    createMethod(
                            "previous",
                            "Return the previous token",
                            new String[] {"exclude_trivia"},
                            new Expr[] {new BooleanLiteral(null, false)},
                            new PrevExpr()),
                    createAttribute("unit", "Return the unit for this token", new UnitExpr()),
                    createAttribute("text", "Return the text of the token", new TextExpr()),
                    createAttribute("kind", "Return the kind of the token", new KindExpr()));

    // ----- Inner classes -----

    /** Expression of the "start_column" method. */
    public static final class StartColExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return (long)
                    LKQLTypeSystemGen.asToken(frame.getArguments()[0])
                            .sourceLocationRange
                            .start
                            .column;
        }
    }

    /** Expression of the "end_column" method. */
    public static final class EndColExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return (long)
                    LKQLTypeSystemGen.asToken(frame.getArguments()[0])
                            .sourceLocationRange
                            .end
                            .column;
        }
    }

    /** Expression of the "start_line" method. */
    public static final class StartLineExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return (long)
                    LKQLTypeSystemGen.asToken(frame.getArguments()[0])
                            .sourceLocationRange
                            .start
                            .line;
        }
    }

    /** Expression of the "end_line" method. */
    public static final class EndLineExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return (long)
                    LKQLTypeSystemGen.asToken(frame.getArguments()[0]).sourceLocationRange.end.line;
        }
    }

    /** Expression of the "is_equivalent" method. */
    public static final class IsEquivalentExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the other token to compare
            Libadalang.Token other;
            try {
                other = LKQLTypeSystemGen.expectToken(frame.getArguments()[1]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.TOKEN,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[1]);
            }

            // Return the comparison
            return LKQLTypeSystemGen.asToken(frame.getArguments()[0]).isEquivalent(other);
        }
    }

    /** Expression of the "is_trivia" method. */
    public static final class IsTriviaExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asToken(frame.getArguments()[0]).triviaIndex != 0;
        }
    }

    /** Expression of the "next" method. */
    public static final class NextExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get if the trivia tokens should be ignored
            boolean ignoreTrivia;
            try {
                ignoreTrivia = LKQLTypeSystemGen.expectBoolean(frame.getArguments()[1]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_BOOLEAN,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[1]);
            }

            Libadalang.Token res = LKQLTypeSystemGen.asToken(frame.getArguments()[0]).next();
            if (ignoreTrivia) {
                while (!res.isNone() && res.triviaIndex != 0) {
                    res = res.next();
                }
            }

            return res;
        }
    }

    /** Expression of the "previous" method. */
    public static final class PrevExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get if the trivia tokens should be ignored
            boolean ignoreTrivia;
            try {
                ignoreTrivia = LKQLTypeSystemGen.expectBoolean(frame.getArguments()[1]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_BOOLEAN,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[1]);
            }

            Libadalang.Token res = LKQLTypeSystemGen.asToken(frame.getArguments()[0]).previous();
            if (ignoreTrivia) {
                while (!res.isNone() && res.triviaIndex != 0) {
                    res = res.previous();
                }
            }

            return res;
        }
    }

    /** Expression of the "unit" method. */
    public static final class UnitExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asToken(frame.getArguments()[0]).unit;
        }
    }

    /** Expression of the "text" method. */
    public static final class TextExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asToken(frame.getArguments()[0]).getText();
        }
    }

    /** Expression of the "kind" method. */
    public static final class KindExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Libadalang.Token token = LKQLTypeSystemGen.asToken(frame.getArguments()[0]);
            if (token.kind.toC() == -1) return "no_token";
            String rawKind = ObjectUtils.toString(token.kind);
            return StringUtils.toLowerCase(StringUtils.split(rawKind, "_")[1]);
        }
    }
}
