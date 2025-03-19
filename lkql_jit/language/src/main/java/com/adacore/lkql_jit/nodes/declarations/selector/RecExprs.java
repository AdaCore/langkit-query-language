//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations.selector;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLRecValue;
import com.adacore.lkql_jit.runtime.values.lists.BaseLKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Idempotent;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Container class: contains every node class for rec expressions in selectors.
 */
public class RecExprs {

    /**
     * Node for rec expression with only one parameter.
     */
    @NodeChild(value = "expr", type = Expr.class)
    public abstract static class UnaryRecExpr extends Expr {

        @CompilerDirectives.CompilationFinal
        private final boolean unpack;

        public UnaryRecExpr(SourceSection location, boolean unpack) {
            super(location);
            this.unpack = unpack;
        }

        @SuppressWarnings("unused")
        @Idempotent
        public boolean hasUnpack() {
            return unpack;
        }

        @Specialization(guards = "hasUnpack()")
        public Object unpack(BaseLKQLList val) {
            return new LKQLRecValue(val.getContent(), val.getContent());
        }

        @Specialization(guards = "!hasUnpack()")
        public Object noUnpack(Object val) {
            Object[] valArray;
            if (LKQLTypeSystemGen.isNullish(val)) {
                valArray = new Object[0];
            } else {
                valArray = new Object[] { val };
            }

            return new LKQLRecValue(valArray, valArray);
        }

        @Fallback
        public Object error(Object val) {
            assert unpack;
            throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_ITERABLE,
                LKQLTypesHelper.fromJava(val),
                this
            );
        }

        @Override
        public String toString(int indentLevel) {
            return null;
        }
    }

    /**
     * Node for one parameter of a rec expression with two parameters (either
     * the recurse part or the result part).
     */
    public abstract static class SubRecExpr extends Node {

        @CompilerDirectives.CompilationFinal
        private final boolean unpack;

        private final SourceSection location;

        @Override
        public SourceSection getSourceSection() {
            return location;
        }

        protected SubRecExpr(SourceSection location, boolean unpack) {
            this.location = location;
            this.unpack = unpack;
        }

        public abstract Object[] execute(Object value);

        @SuppressWarnings("unused")
        @Idempotent
        public boolean hasUnpack() {
            return unpack;
        }

        @Specialization(guards = "hasUnpack()")
        public Object[] unpack(BaseLKQLList val) {
            return val.getContent();
        }

        @Specialization(guards = "!hasUnpack()")
        public Object[] noUnpack(Object val) {
            if (LKQLTypeSystemGen.isNullish(val)) {
                return new Object[0];
            } else {
                return new Object[] { val };
            }
        }

        @Fallback
        public Object[] error(Object val) {
            assert unpack;
            throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_ITERABLE,
                LKQLTypesHelper.fromJava(val),
                this
            );
        }
    }

    /**
     * Rec expr with both a recurse and a result sub-expression.
     */
    public static class BinaryRecExpr extends Expr {

        @Child
        private Expr recurseExpr;

        @Child
        private Expr resultExpr;

        private final SubRecExpr recurseSubRecExpr;
        private final SubRecExpr resultSubRecExpr;

        public BinaryRecExpr(
            SourceSection location,
            boolean recurseHasUnpack,
            Expr recurseExpr,
            boolean resultHasUnpack,
            Expr resultExpr
        ) {
            super(location);
            this.recurseExpr = recurseExpr;
            this.recurseSubRecExpr = RecExprsFactory.SubRecExprNodeGen.create(
                recurseExpr.getSourceSection(),
                recurseHasUnpack
            );
            this.resultSubRecExpr = RecExprsFactory.SubRecExprNodeGen.create(
                resultExpr.getSourceSection(),
                resultHasUnpack
            );

            this.resultExpr = resultExpr;
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            var recurseVal = recurseSubRecExpr.execute(recurseExpr.executeGeneric(frame));
            var resultVal = resultSubRecExpr.execute(resultExpr.executeGeneric(frame));
            return new LKQLRecValue(recurseVal, resultVal);
        }

        @Override
        public String toString(int indentLevel) {
            return null;
        }
    }
}
