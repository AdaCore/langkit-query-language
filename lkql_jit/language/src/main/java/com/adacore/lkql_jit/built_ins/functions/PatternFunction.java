//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.SpecializedBuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.literals.BooleanLiteral;
import com.adacore.lkql_jit.runtime.values.LKQLPattern;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;

/**
 * This class represents the "pattern" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class PatternFunction {

    public static final String NAME = "pattern";

    /** Get a brand new "pattern" function value. */
    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a regex pattern string, create a pattern object",
                new String[] {"regex", "case_sensitive"},
                new Expr[] {null, new BooleanLiteral(null, true)},
                new SpecializedBuiltInBody<>(PatternFunctionFactory.PatternExprNodeGen.create()) {
                    @Override
                    protected Object dispatch(Object[] args) {
                        return specializedNode.executePattern(args[0], args[1]);
                    }
                });
    }

    /** Expression of the "pattern" function. */
    abstract static class PatternExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract LKQLPattern executePattern(Object regex, Object caseSensitive);

        @Specialization
        protected LKQLPattern onValidArgs(String regex, boolean caseSensitive) {
            return new LKQLPattern(body.getCallNode(), regex, caseSensitive);
        }

        @Specialization
        protected LKQLPattern invalidType(String regex, Object notValid) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_BOOLEAN,
                    LKQLTypesHelper.fromJava(notValid),
                    body.argNode(1));
        }

        @Fallback
        protected LKQLPattern invalidType(Object notValid, Object caseSensitive) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_STRING,
                    LKQLTypesHelper.fromJava(notValid),
                    body.argNode(0));
        }
    }
}
