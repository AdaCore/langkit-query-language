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

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.utils.util_functions.ObjectUtils;
import com.adacore.lkql_jit.utils.util_functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This class represents the "img" built-in function in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class ImgFunction implements BuiltInFunction {

    // ----- Attributes -----

    /**
     * The only instance for the "img" built-in
     */
    private static ImgFunction instance = null;

    /**
     * The name of the built-in
     */
    public static final String NAME = "img";

    /**
     * The expression that represents the "img" function execution
     */
    private final ImgExpr imgExpr;

    // ----- Constructors -----

    /**
     * Private constructor
     */
    private ImgFunction() {
        this.imgExpr = new ImgExpr();
    }

    /**
     * Get the instance of the built-in function
     *
     * @return The only instance
     */
    public static ImgFunction getInstance() {
        if (instance == null) {
            instance = new ImgFunction();
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
            "Return a string representation of an object",
            new String[]{"val"},
            new Expr[]{null},
            this.imgExpr
        );
    }

    // ----- Inner classes -----

    /**
     * Expression of the "img" function
     */
    public final static class ImgExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Return the string representation of the argument
            if (frame.getArguments()[0] instanceof String s) {
                return StringUtils.toRepr(s);
            } else {
                return ObjectUtils.toString(frame.getArguments()[0]);
            }
        }
    }

}
