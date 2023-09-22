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

import com.adacore.lkql_jit.LKQLTypeSystem;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This class represents the "concat" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ConcatFunction {

    // ----- Attributes -----

    /**
     * The name of the function.
     */
    public static final String NAME = "concat";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
            NAME,
            "Given a list of lists or strings, return a concatenated list or string",
            new String[]{"lists"},
            new Expr[]{null},
            new ConcatExpr()
        );
    }

    // ------ Inner classes -----

    /**
     * Expression of the "concat" function.
     */
    public final static class ConcatExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the argument
            Object lists = frame.getArguments()[0];

            // Check the type of the argument
            if (!LKQLTypeSystemGen.isListValue(lists)) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_LIST,
                    LKQLTypesHelper.fromJava(lists),
                    this.callNode.getArgList().getArgs()[0]
                );
            }

            // Cast the argument to list
            ListValue listValue = (ListValue) lists;

            // If the list is not empty
            if (listValue.size() > 0) {
                final Object firstItem = listValue.get(0);

                // If the first value is a string look for strings in the list
                if (LKQLTypeSystemGen.isString(firstItem)) {
                    // Create a string builder and add all strings in the list
                    String result = LKQLTypeSystemGen.asString(firstItem);
                    for (int i = 1; i < listValue.size(); i++) {
                        final Object item = listValue.get(i);
                        if (!LKQLTypeSystemGen.isString(item)) {
                            throw LKQLRuntimeException.wrongType(
                                LKQLTypesHelper.LKQL_STRING,
                                LKQLTypesHelper.fromJava(item),
                                this.callNode.getArgList().getArgs()[0]
                            );
                        }
                        result = StringUtils.concat(result, LKQLTypeSystemGen.asString(item));
                    }

                    // Return the result
                    return result;
                }

                // If the first item is a list look for lists in the list
                if (LKQLTypeSystemGen.isListValue(firstItem)) {
                    // Create a result array and add all list of the argument
                    Object[] result = LKQLTypeSystemGen.asListValue(firstItem).getContent();
                    for (int i = 1; i < listValue.size(); i++) {
                        final Object item = listValue.get(i);
                        if (!LKQLTypeSystemGen.isListValue(item)) {
                            throw LKQLRuntimeException.wrongType(
                                LKQLTypesHelper.LKQL_LIST,
                                LKQLTypesHelper.fromJava(item),
                                this.callNode.getArgList().getArgs()[0]
                            );
                        }
                        result = ArrayUtils.concat(
                            result,
                            LKQLTypeSystemGen.asListValue(item).getContent()
                        );
                    }
                    return new ListValue(result);
                }

                // Else there is an error
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.typeUnion(LKQLTypesHelper.LKQL_LIST, LKQLTypesHelper.LKQL_STRING),
                    LKQLTypesHelper.fromJava(firstItem),
                    this.callNode.getArgList().getArgs()[0]
                );
            }

            // If the list is empty just return an empty list
            else {
                return new ListValue(new Object[0]);
            }
        }
    }

}
