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

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.util_functions.ArrayUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.ListValue;


/**
 * This class represents the "concat" built-in function in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class ConcatFunction implements BuiltInFunction {

    // ----- Attributes -----

    /** The only instance of the "concat" built-in */
    private static ConcatFunction instance = null;

    /** The name of the function */
    public static final String NAME = "concat";

    /** The expression that represents the "concat" execution */
    private final ConcatExpr concatExpr;

    // ----- Constructors -----

    /**
     * Private constructor
     */
    private ConcatFunction() {
        this.concatExpr = new ConcatExpr();
    }

    /**
     * Get the instance of the built-in function
     *
     * @return The only instance
     */
    public static ConcatFunction getInstance() {
        if(instance == null) {
            instance = new ConcatFunction();
        }
        return instance;
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.runtime.built_ins.functions.BuiltInFunction#getName() */
    @Override
    public String getName() {
        return NAME;
    }

    /** @see com.adacore.lkql_jit.runtime.built_ins.functions.BuiltInFunction#getValue() */
    @Override
    public BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a list of lists, return a concatenated list",
                new String[]{"lists"},
                new Expr[]{null},
                this.concatExpr
        );
    }

    // ------ Inner classes -----

    /**
     * Expression of the "concat" function
     */
    public final static class ConcatExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the argument
            Object lists = frame.getArguments()[0];

            // Check the type of the argument
            if(!LKQLTypeSystemGen.isListValue(lists)) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_LIST,
                        LKQLTypesHelper.fromJava(lists),
                        this.callNode.getArgList().getArgs()[0]
                );
            }

            // Cast the argument to list
            ListValue listValue = (ListValue) lists;

            // Concatenate the lists inside the list
            Object[] newContent = new Object[0];
            for(int i = 0 ; i < listValue.size() ; i++) {
                if(!LKQLTypeSystemGen.isListValue(listValue.get(i))) {
                    throw LKQLRuntimeException.wrongType(
                            LKQLTypesHelper.LKQL_LIST,
                            LKQLTypesHelper.fromJava(listValue.get(i)),
                            this.callNode.getArgList().getArgs()[0]
                    );
                }

                ListValue currentList = (ListValue) listValue.get(i);
                newContent = ArrayUtils.concat(newContent, currentList.getContent());
            }

            // Return the new list value
            return new ListValue(newContent);
        }
    }

}
