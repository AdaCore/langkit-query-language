/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.runtime.built_ins.functions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.dispatchers.FunctionDispatcher;
import com.adacore.lkql_jit.nodes.dispatchers.FunctionDispatcherNodeGen;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.FunctionValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.utils.Iterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;


/**
 * This class represents the "reduce" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ReduceFunction implements BuiltInFunction {

    // ----- Attributes -----

    /**
     * The only instance of the "reduce" built-in.
     */
    private static ReduceFunction instance = null;

    /**
     * The name of the function.
     */
    public static final String NAME = "reduce";

    /**
     * The expression that represents the "reduce" function execution.
     */
    private final ReduceExpr reduceExpr;

    // ----- Constructors -----

    /**
     * Private constructor.
     */
    private ReduceFunction() {
        this.reduceExpr = new ReduceExpr();
    }

    /**
     * Get the instance of the built-in function.
     *
     * @return The only instance.
     */
    public static ReduceFunction getInstance() {
        if (instance == null) {
            instance = new ReduceFunction();
        }
        return instance;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.built_ins.functions.BuiltInFunction#getName()
     */
    @Override
    public String getName() {
        return NAME;
    }

    /**
     * @see com.adacore.lkql_jit.runtime.built_ins.functions.BuiltInFunction#getValue()
     */
    @Override
    public BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
            NAME,
            "Given a collection, a reduction function, and an initial value reduce the result",
            new String[]{"indexable", "fn", "init"},
            new Expr[]{null, null, null},
            this.reduceExpr
        );
    }

    // ----- Inner classes -----

    /**
     * Expression for the "reduce" function.
     */
    public final static class ReduceExpr extends BuiltInExpr {

        /**
         * The dispatcher for the reduce function.
         */
        @Child
        @SuppressWarnings("FieldMayBeFinal")
        private FunctionDispatcher dispatcher = FunctionDispatcherNodeGen.create();

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            Iterable iterable;
            FunctionValue reduceFunction;
            Object initValue = frame.getArguments()[2];

            try {
                iterable = LKQLTypeSystemGen.expectIterable(frame.getArguments()[0]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_ITERABLE,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.callNode.getArgList().getArgs()[0]
                );
            }

            try {
                reduceFunction = LKQLTypeSystemGen.expectFunctionValue(frame.getArguments()[1]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_FUNCTION,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.callNode.getArgList().getArgs()[1]
                );
            }

            // Verify the function arity
            if (reduceFunction.getParamNames().length != 2) {
                throw LKQLRuntimeException.fromMessage(
                    "Function passed to reduce should have arity of two",
                    this.callNode.getArgList().getArgs()[1]
                );
            }

            // Execute the reducing
            Iterator iterator = iterable.iterator();
            while (iterator.hasNext()) {
                initValue = this.dispatcher.executeDispatch(
                    reduceFunction,
                    new Object[]{reduceFunction.getClosure().getContent(), initValue, iterator.next()}
                );
            }

            // Return the result
            return initValue;
        }
    }

}
