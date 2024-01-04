//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.nodes.expressions.literals.BooleanLiteral;
import com.adacore.lkql_jit.runtime.values.LKQLPattern;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

/**
 * This class represents the "pattern" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class PatternFunction {

    // ----- Attributes -----

    /** The name of the built-in. */
    public static final String NAME = "pattern";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a regex pattern string, create a pattern object",
                new String[] {"regex", "case_sensitive"},
                new Expr[] {null, new BooleanLiteral(null, true)},
                (VirtualFrame frame, FunCall call) -> {
                    // Get the string parameter
                    String regexString;
                    try {
                        regexString = LKQLTypeSystemGen.expectString(frame.getArguments()[0]);
                    } catch (UnexpectedResultException e) {
                        throw LKQLRuntimeException.wrongType(
                                LKQLTypesHelper.LKQL_STRING,
                                LKQLTypesHelper.fromJava(e.getResult()),
                                call.getArgList().getArgs()[0]);
                    }

                    // Get the case sensitiveness parameter
                    boolean caseSensitive;
                    try {
                        caseSensitive = LKQLTypeSystemGen.expectBoolean(frame.getArguments()[1]);
                    } catch (UnexpectedResultException e) {
                        throw LKQLRuntimeException.wrongType(
                                LKQLTypesHelper.LKQL_BOOLEAN,
                                LKQLTypesHelper.fromJava(e.getResult()),
                                call.getArgList().getArgs()[1]);
                    }

                    // Create the pattern and return it
                    return new LKQLPattern(call, regexString, caseSensitive);
                });
    }
}
