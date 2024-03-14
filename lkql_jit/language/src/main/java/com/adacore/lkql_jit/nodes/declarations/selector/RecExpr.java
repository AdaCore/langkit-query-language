/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.nodes.declarations.selector;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.values.LKQLRecValue;
import com.adacore.lkql_jit.built_ins.values.lists.BaseLKQLList;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

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
            SourceLocation location,
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
