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
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
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
 * This class represents the "map" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class MapFunction {

    public static final String NAME = "map";

    /** Get a brand new "map" function value. */
    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given an iterable object and a function, return the list resulting of the "
                        + "function application on each element of the iterable object: "
                        + "map(lst, f) -> [f(lst[1]), f(lst[2]), ...]",
                new String[] {"indexable", "fn"},
                new Expr[] {null, null},
                new SpecializedBuiltInBody<>(MapFunctionFactory.MaxExprNodeGen.create()) {
                    @Override
                    protected Object dispatch(Object[] args) {
                        return specializedNode.executeMap(args[0], args[1]);
                    }
                });
    }

    /** Expression of the "map" function. */
    abstract static class MaxExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract LKQLList executeMap(Object iterable, Object function);

        @Specialization(
                limit = Constants.SPECIALIZED_LIB_LIMIT,
                guards = "function.parameterNames.length == 1")
        protected LKQLList onValidArgs(
                Iterable iterable,
                LKQLFunction function,
                @CachedLibrary("function") InteropLibrary functionLibrary) {
            Object[] res = new Object[(int) iterable.size()];
            int i = 0;
            Iterator iterator = iterable.iterator();

            while (iterator.hasNext()) {
                try {
                    res[i] =
                            functionLibrary.execute(
                                    function, function.closure.getContent(), iterator.next());
                } catch (ArityException
                        | UnsupportedTypeException
                        | UnsupportedMessageException e) {
                    // TODO: Implement runtime checks in the LKQLFunction class and base computing
                    // on them (#138)
                    throw LKQLRuntimeException.fromJavaException(e, body.argNode(1));
                }
                i++;
            }

            return new LKQLList(res);
        }

        @Specialization
        protected LKQLList onInvalidFunction(Iterable iterable, LKQLFunction function) {
            throw LKQLRuntimeException.wrongArity(
                    1, function.parameterNames.length, body.argNode(1));
        }

        @Specialization
        protected LKQLList invalidType(Iterable iterable, Object notValid) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_FUNCTION,
                    LKQLTypesHelper.fromJava(notValid),
                    body.argNode(1));
        }

        @Fallback
        protected LKQLList invalidType(Object notValid, Object function) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_ITERABLE,
                    LKQLTypesHelper.fromJava(notValid),
                    body.argNode(0));
        }
    }
}
