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
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This class represents the "doc" built-in function in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class DocFunction implements BuiltInFunction {

    // ----- Attributes -----

    /**
     * The only instance of the "doc" built-in
     */
    private static DocFunction instance = null;

    /**
     * The name of the function
     */
    public static final String NAME = "doc";

    /**
     * The expression that represents the "doc" function execution
     */
    private final DocExpr docExpr;

    // ----- Constructors -----

    /**
     * Private constructor
     */
    private DocFunction() {
        this.docExpr = new DocExpr();
    }

    /**
     * Get the instance of the built-in function
     *
     * @return The only instance
     */
    public static DocFunction getInstance() {
        if (instance == null) {
            instance = new DocFunction();
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
            "Given any object, return the documentation associated with it",
            new String[]{"obj"},
            new Expr[]{null},
            this.docExpr
        );
    }

    // ----- Inner classes -----

    /**
     * Expression of the "doc" function
     */
    public final static class DocExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the argument
            Object arg = frame.getArguments()[0];

            // If the argument is an LKQL value, read the documentation from ir
            if (LKQLTypeSystemGen.isLKQLValue(arg)) {
                return ((LKQLValue) arg).getDocumentation();
            }

            // Return the default empty documentation
            return "";
        }
    }

}
