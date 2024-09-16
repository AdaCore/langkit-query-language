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
import com.adacore.lkql_jit.built_ins.SpecializedBuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.literals.BooleanLiteral;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
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
                            new SpecializedBuiltInBody<>(
                                    TokenMethodsFactory.IsEquivalentExprNodeGen.create()) {
                                @Override
                                protected Object dispatch(Object[] args) {
                                    return this.specializedNode.executeIsEquivalent(
                                            LKQLTypeSystemGen.asToken(args[0]), args[1]);
                                }
                            }),
                    createAttribute(
                            "is_trivia",
                            "Return whether this token is a trivia",
                            new IsTriviaExpr()),
                    createMethod(
                            "next",
                            "Return the next token",
                            new String[] {"exclude_trivia"},
                            new Expr[] {new BooleanLiteral(null, false)},
                            new SpecializedBuiltInBody<>(
                                    TokenMethodsFactory.NextExprNodeGen.create()) {
                                @Override
                                protected Object dispatch(Object[] args) {
                                    return this.specializedNode.executeNext(
                                            LKQLTypeSystemGen.asToken(args[0]), args[1]);
                                }
                            }),
                    createMethod(
                            "previous",
                            "Return the previous token",
                            new String[] {"exclude_trivia"},
                            new Expr[] {new BooleanLiteral(null, false)},
                            new SpecializedBuiltInBody<>(
                                    TokenMethodsFactory.PrevExprNodeGen.create()) {
                                @Override
                                protected Object dispatch(Object[] args) {
                                    return this.specializedNode.executePrev(
                                            LKQLTypeSystemGen.asToken(args[0]), args[1]);
                                }
                            }),
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
    abstract static class IsEquivalentExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract boolean executeIsEquivalent(Libadalang.Token receiver, Object other);

        @Specialization
        protected boolean onValid(Libadalang.Token receiver, Libadalang.Token other) {
            return receiver.isEquivalent(other);
        }

        @Fallback
        protected boolean onInvalid(
                @SuppressWarnings("unused") Libadalang.Token receiver, Object other) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.TOKEN, LKQLTypesHelper.fromJava(other), this.body.argNode(0));
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
    abstract static class NextExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract Libadalang.Token executeNext(
                Libadalang.Token receiver, Object ignoreTrivia);

        @Specialization
        protected Libadalang.Token onValid(Libadalang.Token receiver, boolean ignoreTrivia) {
            // Skip trivia if required
            Libadalang.Token res = receiver.next();
            if (ignoreTrivia) {
                while (!res.isNone() && res.triviaIndex != 0) {
                    res = res.next();
                }
            }

            // Return the result
            return res;
        }

        @Fallback
        protected Libadalang.Token onInvalid(
                @SuppressWarnings("unused") Libadalang.Token receiver, Object ignoreTrivia) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_BOOLEAN,
                    LKQLTypesHelper.fromJava(ignoreTrivia),
                    this.body.argNode(0));
        }
    }

    /** Expression of the "previous" method. */
    abstract static class PrevExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract Libadalang.Token executePrev(
                Libadalang.Token receiver, Object ignoreTrivia);

        @Specialization
        protected Libadalang.Token onValid(Libadalang.Token receiver, boolean ignoreTrivia) {
            // Skip trivia if required
            Libadalang.Token res = receiver.previous();
            if (ignoreTrivia) {
                while (!res.isNone() && res.triviaIndex != 0) {
                    res = res.previous();
                }
            }

            // Return the result
            return res;
        }

        @Fallback
        protected Libadalang.Token onInvalid(
                @SuppressWarnings("unused") Libadalang.Token receiver, Object ignoreTrivia) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_BOOLEAN,
                    LKQLTypesHelper.fromJava(ignoreTrivia),
                    this.body.argNode(0));
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
