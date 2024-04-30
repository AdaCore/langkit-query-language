//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations.selector;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.values.LKQLRecValue;
import com.adacore.lkql_jit.built_ins.values.lists.BaseLKQLList;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class RecExpr extends Expr {

    private final boolean recurseHasUnpack;

    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr recurseExpr;

    private final boolean resultHasUnpack;

    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr resultExpr;

    public RecExpr(
            SourceSection location,
            boolean recurseHasUnpack,
            Expr recurseExpr,
            boolean resultHasUnpack,
            Expr resultExpr) {
        super(location);
        this.recurseHasUnpack = recurseHasUnpack;
        this.recurseExpr = recurseExpr;
        this.resultHasUnpack = resultHasUnpack;
        this.resultExpr = resultExpr;
    }

    public Object executeGeneric(VirtualFrame frame) {
        final var recurseExprVal = this.recurseExpr.executeGeneric(frame);

        Object[] recurseVal = null;
        if (this.recurseHasUnpack) {
            if (!LKQLTypeSystemGen.isBaseLKQLList(recurseExprVal)) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_ITERABLE,
                        LKQLTypesHelper.fromJava(recurseExprVal),
                        this.recurseExpr);
            }
            BaseLKQLList i = (BaseLKQLList) recurseExprVal;
            recurseVal = i.getContent();
        } else if (!LKQLTypeSystemGen.isNullish(recurseExprVal)) {
            recurseVal = new Object[1];
            recurseVal[0] = recurseExprVal;
        } else {
            recurseVal = new Object[0];
        }

        // By default, if not specified, the result is the same as the recurse
        // NOTE: we also need to propagate the unpack information if the recurse is unspecified
        Object resultExprVal = null;
        Boolean resultHasUnpack = this.resultHasUnpack;
        if (this.resultExpr == null) {
            resultExprVal = recurseExprVal;
            resultHasUnpack = this.recurseHasUnpack;
        } else {
            resultExprVal = this.resultExpr.executeGeneric(frame);
        }

        Object[] resultVal = null;
        if (resultHasUnpack) {
            if (!LKQLTypeSystemGen.isBaseLKQLList(resultExprVal)) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_ITERABLE,
                        LKQLTypesHelper.fromJava(resultExprVal),
                        this.resultExpr);
            }
            BaseLKQLList i = (BaseLKQLList) resultExprVal;
            resultVal = i.getContent();
        } else if (!LKQLTypeSystemGen.isNullish(resultExprVal)) {
            resultVal = new Object[1];
            resultVal[0] = resultExprVal;
        } else {
            resultVal = new Object[0];
        }

        return new LKQLRecValue(recurseVal, resultVal);
    }

    @Override
    public String toString(int indentLevel) {
        return null;
    }
}
