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
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Indexable;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.util_functions.ArrayUtils;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This class represents the "unique" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class UniqueFunction implements BuiltInFunction {

    // ----- Attributes -----

    /**
     * The only instance of the "unique" built-in.
     */
    private static UniqueFunction instance = null;

    /**
     * The name of the function.
     */
    public static final String NAME = "unique";

    /**
     * The expression that represents the "unique" function execution.
     */
    private final UniqueExpr uniqueExpr;

    // ----- Constructors -----

    /**
     * Private constructor.
     */
    private UniqueFunction() {
        this.uniqueExpr = new UniqueExpr();
    }

    /**
     * Get the instance of the built-in function.
     *
     * @return The only instance.
     */
    public static UniqueFunction getInstance() {
        if (instance == null) {
            instance = new UniqueFunction();
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
            "Given collection, remove all identical elements in order to have only one instance of each",
            new String[]{"indexable"},
            new Expr[]{null},
            this.uniqueExpr
        );
    }

    // ----- Inner classes -----

    /**
     * Expression of the "unique" function.
     */
    public final static class UniqueExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the argument
            Object indexableObject = frame.getArguments()[0];

            // Verify the argument type
            if (!LKQLTypeSystemGen.isIndexable(indexableObject)) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_LIST,
                    LKQLTypesHelper.fromJava(indexableObject),
                    this.callNode.getArgList().getArgs()[0]
                );
            }

            // Cast the argument
            Indexable indexable = LKQLTypeSystemGen.asIndexable(indexableObject);

            Object[] res = ArrayUtils.unique(indexable.getContent());

            // Return the result list
            return new ListValue(ArrayUtils.unique(indexable.getContent()));
        }
    }

}
