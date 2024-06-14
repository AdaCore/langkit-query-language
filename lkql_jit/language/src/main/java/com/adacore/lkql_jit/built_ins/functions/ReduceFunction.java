//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.SpecializedBuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.Iterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;

/**
 * This class represents the "reduce" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ReduceFunction {

    public static final String NAME = "reduce";

    /** Get a brand new "reduce" function value. */
    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a collection, a reduction function, and an initial value reduce the result",
                new String[] {"indexable", "fn", "init"},
                new Expr[] {null, null, null},
                new SpecializedBuiltInBody<>(ReduceFunctionFactory.ReduceExprNodeGen.create()) {
                    @Override
                    protected Object dispatch(Object[] args) {
                        return specializedNode.executeReduce(args[0], args[1], args[2]);
                    }
                });
    }

    /** Expression of the "reduce" function. */
    abstract static class ReduceExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract Object executeReduce(Object iterable, Object function, Object initValue);

        @Specialization(
                limit = Constants.SPECIALIZED_LIB_LIMIT,
                guards = "function.parameterNames.length == 2")
        protected Object onValidArgs(
                Iterable iterable,
                LKQLFunction function,
                Object initValue,
                @CachedLibrary("function") InteropLibrary functionLibrary) {
            Iterator iterator = iterable.iterator();
            while (iterator.hasNext()) {
                try {
                    initValue =
                            functionLibrary.execute(
                                    function,
                                    function.closure.getContent(),
                                    initValue,
                                    iterator.next());
                } catch (ArityException
                        | UnsupportedTypeException
                        | UnsupportedMessageException e) {
                    // TODO: Implement runtime checks in the LKQLFunction class and base computing
                    // on them (#138)
                    throw LKQLRuntimeException.fromJavaException(e, body.argNode(1));
                }
            }
            return initValue;
        }

        @Specialization
        protected Object onInvalidFunction(
                Iterable iterable, LKQLFunction function, Object initValue) {
            throw LKQLRuntimeException.wrongArity(
                    2, function.parameterNames.length, body.argNode(1));
        }

        @Specialization
        protected Object invalidType(Iterable iterable, Object notValid, Object initValue) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_FUNCTION,
                    LKQLTypesHelper.fromJava(notValid),
                    body.argNode(1));
        }

        @Fallback
        protected Object invalidType(Object notValid, Object function, Object initValue) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_ITERABLE,
                    LKQLTypesHelper.fromJava(notValid),
                    body.argNode(0));
        }
    }
}
