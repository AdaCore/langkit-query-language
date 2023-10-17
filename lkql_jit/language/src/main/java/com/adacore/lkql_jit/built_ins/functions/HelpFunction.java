/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.values.LKQLUnit;
import com.adacore.lkql_jit.built_ins.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class represents "help" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class HelpFunction {

    // ----- Attributes -----

    /** The name of the function. */
    public static final String NAME = "help";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given any object, return formatted help for it",
                new String[] {"obj"},
                new Expr[] {null},
                (VirtualFrame frame, FunCall call) -> {
                    // Get the argument
                    Object arg = frame.getArguments()[0];

                    // If the argument is an LKQL value, read the documentation from ir
                    if (LKQLTypeSystemGen.isLKQLValue(arg)) {
                        LKQLValue value = LKQLTypeSystemGen.asLKQLValue(arg);
                        LKQLLanguage.getContext(call)
                                .println(
                                        StringUtils.concat(
                                                value.getProfile(),
                                                "\n",
                                                value.getDocumentation()));
                    }

                    // Return the default empty documentation
                    return LKQLUnit.INSTANCE;
                });
    }
}
