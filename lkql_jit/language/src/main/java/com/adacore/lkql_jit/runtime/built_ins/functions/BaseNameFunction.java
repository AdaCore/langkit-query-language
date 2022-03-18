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
import com.adacore.lkql_jit.utils.util_functions.FileUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;


/**
 * This class represents the "base_name" built-in function in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class BaseNameFunction implements BuiltInFunction {

    // ----- Attributes -----

    /** The only instance of the "base_name" function */
    private static BaseNameFunction instance = null;

    /** The name of the built-in */
    public static final String NAME = "base_name";

    /** The expression that represents the "base_name" execution */
    private final BaseNameExpr baseNameExpr;

    // ----- Constructors -----

    /**
     * Private constructor
     */
    private BaseNameFunction() {
        this.baseNameExpr = new BaseNameExpr();
    }

    /**
     * Get the instance of the built-in function
     *
     * @return The only instance
     */
    public static BaseNameFunction getInstance() {
        if(instance == null) {
            instance = new BaseNameFunction();
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
                "Given a string that represents a file name, returns the basename",
                new String[]{"str"},
                new Expr[]{null},
                this.baseNameExpr
        );
    }

    // ----- Inner classes -----

    /**
     * Expression of the "base_name" function
     */
    public final static class BaseNameExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the file full path
            Object path = frame.getArguments()[0];

            // Check the argument type
            if(!LKQLTypeSystemGen.isString(path)) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_STRING,
                        LKQLTypesHelper.fromJava(path),
                        this.callNode.getArgList().getArgs()[0]
                );
            }

            // Return the base name of the file
            return FileUtils.baseName(LKQLTypeSystemGen.asString(path));
        }
    }

}
