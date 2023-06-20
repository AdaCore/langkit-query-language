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

package com.adacore.lkql_jit.runtime.built_ins.methods;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.literals.BooleanLiteral;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.util_functions.ObjectUtils;
import com.adacore.lkql_jit.utils.util_functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;


/**
 * This class contains all built-in methods for the token type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class TokenMethods extends CommonMethods {

    // ----- Attributes -----

    /**
     * The only instance of the method collection.
     */
    private static TokenMethods instance = null;

    // ----- Constructors -----

    /**
     * Private constructor.
     */
    private TokenMethods() {
        super();
    }

    /**
     * Get the only instance of the token method library.
     *
     * @return The instance of the token methods.
     */
    public static TokenMethods getInstance() {
        if (instance == null) {
            instance = new TokenMethods();
        }
        return instance;
    }

    /**
     * @see com.adacore.lkql_jit.runtime.built_ins.methods.CommonMethods#initMethods()
     */
    @Override
    protected void initMethods() {
        super.initMethods();
        this.methods.put("start_column", new BuiltInFunctionValue(
            "start_column",
            "Return the column start",
            new String[]{"token"},
            new Expr[]{null},
            new StartColExpr()
        ));
        this.methods.put("end_column", new BuiltInFunctionValue(
            "end_column",
            "Return the column end",
            new String[]{"token"},
            new Expr[]{null},
            new EndColExpr()
        ));
        this.methods.put("start_line", new BuiltInFunctionValue(
            "start_line",
            "Return the line start",
            new String[]{"token"},
            new Expr[]{null},
            new StartLineExpr()
        ));
        this.methods.put("end_line", new BuiltInFunctionValue(
            "end_line",
            "Return the line end",
            new String[]{"token"},
            new Expr[]{null},
            new EndLineExpr()
        ));
        this.methods.put("is_equivalent", new BuiltInFunctionValue(
            "is_equivalent",
            "Return whether two tokens are structurally equivalent",
            new String[]{"this", "other"},
            new Expr[]{null, null},
            new IsEquivalentExpr()
        ));
        this.methods.put("is_trivia", new BuiltInFunctionValue(
            "is_trivia",
            "Return whether this token is a trivia",
            new String[]{"token"},
            new Expr[]{null},
            new IsTriviaExpr()
        ));
        this.methods.put("next", new BuiltInFunctionValue(
            "next",
            "Return the next token",
            new String[]{"token", "exclude_trivia"},
            new Expr[]{null, new BooleanLiteral(null, false)},
            new NextExpr()
        ));
        this.methods.put("previous", new BuiltInFunctionValue(
            "previous",
            "Return the previous token",
            new String[]{"token", "exclude_trivia"},
            new Expr[]{null, new BooleanLiteral(null, false)},
            new PrevExpr()
        ));
        this.methods.put("unit", new BuiltInFunctionValue(
            "unit",
            "Return the unit for this token",
            new String[]{"token"},
            new Expr[]{null},
            new UnitExpr()
        ));
        this.methods.put("text", new BuiltInFunctionValue(
            "text",
            "Return the text of the token",
            new String[]{"token"},
            new Expr[]{null},
            new TextExpr()
        ));
        this.methods.put("kind", new BuiltInFunctionValue(
            "kind",
            "Return the kind of the token",
            new String[]{"token"},
            new Expr[]{null},
            new KindExpr()
        ));
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.built_ins.methods.BuiltInMethods#getType()
     */
    @Override
    public String getType() {
        return LKQLTypesHelper.TOKEN;
    }

    // ----- Inner classes -----

    /**
     * Expression of the "start_column" method.
     */
    public static final class StartColExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return (long) LKQLTypeSystemGen.asToken(frame.getArguments()[0]).sourceLocationRange.start.column;
        }
    }

    /**
     * Expression of the "end_column" method.
     */
    public static final class EndColExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return (long) LKQLTypeSystemGen.asToken(frame.getArguments()[0]).sourceLocationRange.end.column;
        }
    }

    /**
     * Expression of the "start_line" method.
     */
    public static final class StartLineExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return (long) LKQLTypeSystemGen.asToken(frame.getArguments()[0]).sourceLocationRange.start.line;
        }
    }

    /**
     * Expression of the "end_line" method.
     */
    public static final class EndLineExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return (long) LKQLTypeSystemGen.asToken(frame.getArguments()[0]).sourceLocationRange.end.line;
        }
    }

    /**
     * Expression of the "is_equivalent" method.
     */
    public static final class IsEquivalentExpr extends BuiltInExpr {
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
                    this.callNode.getArgList().getArgs()[1]
                );
            }

            // Return the comparison
            return LKQLTypeSystemGen.asToken(frame.getArguments()[0]).isEquivalent(other);
        }
    }

    /**
     * Expression of the "is_trivia" method.
     */
    public static final class IsTriviaExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asToken(frame.getArguments()[0]).triviaIndex != 0;
        }
    }

    /**
     * Expression of the "next" method.
     */
    public static final class NextExpr extends BuiltInExpr {
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
                    this.callNode.getArgList().getArgs()[1]
                );
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

    /**
     * Expression of the "previous" method.
     */
    public static final class PrevExpr extends BuiltInExpr {
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
                    this.callNode.getArgList().getArgs()[1]
                );
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

    /**
     * Expression of the "unit" method.
     */
    public static final class UnitExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asToken(frame.getArguments()[0]).unit;
        }
    }

    /**
     * Expression of the "text" method.
     */
    public static final class TextExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asToken(frame.getArguments()[0]).getText();
        }
    }

    /**
     * Expression of the "kind" method.
     */
    public static final class KindExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Libadalang.Token token = LKQLTypeSystemGen.asToken(frame.getArguments()[0]);
            if (token.kind.toC() == -1) return "no_token";
            String rawKind = ObjectUtils.toString(token.kind);
            return StringUtils.toLowerCase(StringUtils.split(
                rawKind, "_"
            )[1]);
        }
    }

}
