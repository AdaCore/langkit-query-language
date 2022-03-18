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

import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;


/**
 * This class represents the "profile" built-in function in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class ProfileFunction implements BuiltInFunction {

    // ----- Attributes -----

    /** The only instance of the "profile" built-in */
    private static ProfileFunction instance = null;

    /** The name of the function */
    public static final String NAME = "profile";

    /** The expression that represents the "profile" function execution */
    private final ProfileExpr profileExpr;

    // ----- Constructors -----

    /**
     * Private constructor
     */
    private ProfileFunction() {
        this.profileExpr = new ProfileExpr();
    }

    /**
     * Get the instance of the built-in function
     *
     * @return The only instance
     */
    public static ProfileFunction getInstance() {
        if(instance == null) {
            instance = new ProfileFunction();
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
                "Given any object, if it is a callable, return its profile as text",
                new String[]{"obj"},
                new Expr[]{null},
                this.profileExpr
        );
    }

    // ----- Inner classes -----

    /**
     * Expression of the "profile" function
     */
    public final static class ProfileExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the argument
            Object arg = frame.getArguments()[0];

            // If the argument is an LKQL value, read the documentation from ir
            if(LKQLTypeSystemGen.isLKQLValue(arg)) {
                return ((LKQLValue) arg).getProfile();
            }

            // Return the default empty documentation
            return "";
        }
    }

}
