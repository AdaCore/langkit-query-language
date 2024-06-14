//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.SpecializedBuiltInBody;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;

/**
 * This class represents the "img" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ImgFunction {

    public static final String NAME = "img";

    /** Get a brand new "img" function value. */
    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Return a string representation of an object",
                new String[] {"val"},
                new Expr[] {null},
                new SpecializedBuiltInBody<>(ImgFunctionFactory.ImgExprNodeGen.create()) {
                    @Override
                    protected Object dispatch(Object[] args) {
                        return specializedNode.executeImg(args[0]);
                    }
                });
    }

    /** Expression of the "img" function. */
    abstract static class ImgExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract String executeImg(Object obj);

        @Specialization
        protected String onString(String string) {
            return StringUtils.toRepr(string);
        }

        @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
        protected String onObject(Object obj, @CachedLibrary("obj") InteropLibrary objLibrary) {
            return (String) objLibrary.toDisplayString(obj);
        }
    }
}
