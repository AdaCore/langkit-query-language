//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.SpecializedBuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.literals.BooleanLiteral;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;

/**
 * This class represents the "print" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class PrintFunction {

    public static final String NAME = "print";

    /** Get a brand new "print" function value. */
    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Built-in print function. Prints whatever is passed as an argument",
                new String[] {"val", "new_line"},
                new Expr[] {null, new BooleanLiteral(null, true)},
                new SpecializedBuiltInBody<>(PrintFunctionFactory.PrintExprNodeGen.create()) {
                    @Override
                    protected Object dispatch(Object[] args) {
                        return specializedNode.executePrint(args[0], args[1]);
                    }
                });
    }

    /** Expression of the "print" function. */
    abstract static class PrintExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract LKQLUnit executePrint(Object toPrint, Object newline);

        @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
        protected LKQLUnit onBoolean(
                Object toPrint,
                boolean newline,
                @CachedLibrary("toPrint") InteropLibrary printingLibrary) {
            if (newline) {
                LKQLLanguage.getContext(null)
                        .println((String) printingLibrary.toDisplayString(toPrint));
            } else {
                LKQLLanguage.getContext(null)
                        .print((String) printingLibrary.toDisplayString(toPrint));
            }
            return LKQLUnit.INSTANCE;
        }

        @Fallback
        protected LKQLUnit invalidType(Object toPrint, Object notValid) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_BOOLEAN,
                    LKQLTypesHelper.fromJava(notValid),
                    body.argNode(1));
        }
    }
}
