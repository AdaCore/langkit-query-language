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
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.runtime.built_ins.BuiltinFunctionBody;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.FileUtils;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This class represents the "base_name" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class BaseNameFunction {

    // ----- Attributes -----

    /**
     * The name of the built-in.
     */
    public static final String NAME = "base_name";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
            NAME,
            "Given a string that represents a file name, returns the basename",
            new String[]{"str"},
            new Expr[]{null},
            (VirtualFrame frame, FunCall call) -> {
                // Get the file full path
                Object path = frame.getArguments()[0];

                // Check the argument type
                if (!LKQLTypeSystemGen.isString(path)) {
                    throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_STRING,
                        LKQLTypesHelper.fromJava(path),
                        call.getArgList().getArgs()[0]
                    );
                }

                // Return the base name of the file
                return FileUtils.baseName(LKQLTypeSystemGen.asString(path));
            }
        );
    }

}
