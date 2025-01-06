//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.SpecializedBuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.FileUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;

/**
 * This class represents the "base_name" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class BaseNameFunction {

    public static final String NAME = "base_name";

    /** Get a brand new "base_name" function value. */
    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a string that represents a file name, returns the basename",
                new String[] {"str"},
                new Expr[] {null},
                new SpecializedBuiltInBody<>(BaseNameFunctionFactory.BaseNameExprNodeGen.create()) {
                    @Override
                    protected Object dispatch(Object[] args) {
                        return specializedNode.executeBaseName(args[0]);
                    }
                });
    }

    /** Expression of the "base_name" function. */
    abstract static class BaseNameExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract String executeBaseName(Object fileName);

        @Specialization
        protected String executeOnString(String fileName) {
            return FileUtils.baseName(fileName);
        }

        @Fallback
        protected String invalidType(Object notValid) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_STRING,
                    LKQLTypesHelper.fromJava(notValid),
                    body.argNode(0));
        }
    }
}
