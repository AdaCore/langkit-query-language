//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.AbstractBuiltInFunctionBody;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.values.LKQLFunction;
import com.adacore.lkql_jit.built_ins.values.interfaces.Iterable;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.Iterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

/**
 * This class represents the "reduce" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ReduceFunction {

    // ----- Attributes -----

    /** The name of the function. */
    public static final String NAME = "reduce";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a collection, a reduction function, and an initial value reduce the result",
                new String[] {"indexable", "fn", "init"},
                new Expr[] {null, null, null},
                new ReduceExpr());
    }

    // ----- Inner classes -----

    /** Expression for the "reduce" function. */
    public static final class ReduceExpr extends AbstractBuiltInFunctionBody {

        /** An uncached interop library for the checker functions execution. */
        private InteropLibrary interopLibrary = InteropLibrary.getUncached();

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            Iterable iterable;
            LKQLFunction reduceFunction;
            Object initValue = frame.getArguments()[2];

            try {
                iterable = LKQLTypeSystemGen.expectIterable(frame.getArguments()[0]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_ITERABLE,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[0]);
            }

            try {
                reduceFunction = LKQLTypeSystemGen.expectLKQLFunction(frame.getArguments()[1]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_FUNCTION,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[1]);
            }

            // Verify the function arity
            if (reduceFunction.parameterNames.length != 2) {
                throw LKQLRuntimeException.fromMessage(
                        "Function passed to reduce should have arity of two",
                        this.callNode.getArgList().getArgs()[1]);
            }

            // Execute the reducing
            Iterator iterator = iterable.iterator();
            while (iterator.hasNext()) {
                try {
                    initValue =
                            this.interopLibrary.execute(
                                    reduceFunction,
                                    reduceFunction.closure.getContent(),
                                    initValue,
                                    iterator.next());
                } catch (ArityException
                        | UnsupportedTypeException
                        | UnsupportedMessageException e) {
                    // TODO: Implement runtime checks in the LKQLFunction class and base computing
                    // on them (#138)
                    throw LKQLRuntimeException.fromJavaException(e, this.callNode);
                }
            }

            // Return the result
            return initValue;
        }
    }
}
