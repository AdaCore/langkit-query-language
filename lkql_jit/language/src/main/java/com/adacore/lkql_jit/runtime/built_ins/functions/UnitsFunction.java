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

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This class represents the "units" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class UnitsFunction {

    // ----- Attributes -----

    /**
     * The name of the built-in.
     */
    public static final String NAME = "units";

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
            NAME,
            "Return an iterator on all units",
            new String[]{},
            new Expr[]{},
            new UnitsExpr()
        );
    }

    // ----- Inner classes -----

    /**
     * Expression of the "units" function.
     */
    public static final class UnitsExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return new ListValue(LKQLLanguage.getContext(this).getAllUnits());
        }
    }

}
