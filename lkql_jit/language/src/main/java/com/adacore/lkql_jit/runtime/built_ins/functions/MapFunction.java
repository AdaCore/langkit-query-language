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
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.util_classes.Iterator;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;


/**
 * This class represents the "map" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class MapFunction implements BuiltInFunction {

    // ----- Attributes -----

    /**
     * The only instance of the "map" built-in.
     */
    private static MapFunction instance = null;

    /**
     * The name of the function.
     */
    public static final String NAME = "map";

    /**
     * The expression that represents the "map" function execution.
     */
    private final MapFunction.MapExpr mapExpr;

    // ----- Constructors -----

    /**
     * This private constructor.
     */
    public MapFunction() {
        this.mapExpr = new MapExpr();
    }

    /**
     * Get the instance of the built-in function.
     *
     * @return The only instance.
     */
    public static MapFunction getInstance() {
        if (instance == null) {
            instance = new MapFunction();
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
            "Given a collection, a mapping function",
            new String[]{"indexable", "fn"},
            new Expr[]{null, null},
            this.mapExpr
        );
    }

    // ----- Inner classes -----

    /**
     * Expression of the "map" function.
     */
    public static final class MapExpr extends BuiltInExpr {

        /**
         * The dispatcher for the mapping function.
         */
        @Child
        @SuppressWarnings("FieldMayBeFinal")
        private FunctionDispatcher dispatcher = FunctionDispatcherNodeGen.create();

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            Iterable iterable;
            FunctionValue mapFunction;

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
                mapFunction = LKQLTypeSystemGen.expectFunctionValue(frame.getArguments()[1]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_FUNCTION,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.callNode.getArgList().getArgs()[1]
                );
            }

            // Verify the function arrity
            if (mapFunction.getParamNames().length != 1) {
                throw LKQLRuntimeException.fromMessage(
                    "Function passed to map should have arity of one",
                    this.callNode.getArgList().getArgs()[1]
                );
            }

            // Prepare the result
            Object[] res = new Object[(int) iterable.size()];

            // Apply the mapping function
            int i = 0;
            Iterator iterator = iterable.iterator();
            while (iterator.hasNext()) {
                res[i] = this.dispatcher.executeDispatch(
                    mapFunction,
                    new Object[]{mapFunction.getClosure().getContent(), iterator.next()}
                );
                i++;
            }

            // Return the result
            return new ListValue(res);
        }

    }

}
