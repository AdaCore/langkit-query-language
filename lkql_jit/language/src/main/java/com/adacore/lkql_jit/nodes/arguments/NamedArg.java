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

package com.adacore.lkql_jit.nodes.arguments;

import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;


/**
 * This node represents a named argument in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class NamedArg extends Arg {

    // ----- Constructor -----

    /**
     * Create a new named argument node
     *
     * @param location The location of the node in the sources
     * @param name     The name of the argument
     * @param argExpr  The expression of the argument
     */
    public NamedArg(
        SourceLocation location,
        Identifier name,
        Expr argExpr
    ) {
        super(location, name, argExpr);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return nodeRepresentation(
            indentLevel,
            new String[]{"name"},
            new Object[]{this.argName}
        );
    }

}
