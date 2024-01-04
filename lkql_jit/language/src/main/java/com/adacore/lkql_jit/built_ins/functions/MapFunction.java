//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.AbstractBuiltInFunctionBody;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.Iterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

/**
 * This class represents the "map" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class MapFunction {

    // ----- Attributes -----

    /** The name of the function. */
    public static final String NAME = "map";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a collection, a mapping function",
                new String[] {"indexable", "fn"},
                new Expr[] {null, null},
                new MapExpr());
    }

    // ----- Inner classes -----

    /** Expression of the "map" function. */
    public static final class MapExpr extends AbstractBuiltInFunctionBody {

        /** An uncached interop library for the checker functions execution. */
        private InteropLibrary interopLibrary = InteropLibrary.getUncached();

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            Iterable iterable;
            LKQLFunction mapFunction;

            try {
                iterable = LKQLTypeSystemGen.expectIterable(frame.getArguments()[0]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_ITERABLE,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[0]);
            }

            try {
                mapFunction = LKQLTypeSystemGen.expectLKQLFunction(frame.getArguments()[1]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_FUNCTION,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[1]);
            }

            // Verify the function arity
            if (mapFunction.parameterNames.length != 1) {
                throw LKQLRuntimeException.fromMessage(
                        "Function passed to map should have arity of one",
                        this.callNode.getArgList().getArgs()[1]);
            }

            // Prepare the result
            Object[] res = new Object[(int) iterable.size()];

            // Apply the mapping function
            int i = 0;
            Iterator iterator = iterable.iterator();
            while (iterator.hasNext()) {
                try {
                    res[i] =
                            this.interopLibrary.execute(
                                    mapFunction, mapFunction.closure.getContent(), iterator.next());
                } catch (ArityException
                        | UnsupportedTypeException
                        | UnsupportedMessageException e) {
                    // TODO: Implement runtime checks in the LKQLFunction class and base computing
                    // on them (#138)
                    throw LKQLRuntimeException.fromJavaException(e, this.callNode);
                }
                i++;
            }

            // Return the result
            return new LKQLList(res);
        }
    }
}
