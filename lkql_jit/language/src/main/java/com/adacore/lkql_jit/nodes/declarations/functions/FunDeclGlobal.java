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

package com.adacore.lkql_jit.nodes.declarations.functions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.expressions.FunExpr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.lkql_jit.nodes.declarations.DeclAnnotation;
import com.adacore.lkql_jit.runtime.values.FunctionValue;
import com.adacore.lkql_jit.runtime.values.UnitValue;


/**
 * This node represents a global function declaration in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class FunDeclGlobal extends FunDecl {

    // ----- Constructors -----

    /**
     * Create a new global function declaration node
     *
     * @param location The location of the node in the sources
     * @param annotation The function annotation
     * @param name The name of the function
     * @param slot The slot to put the function in
     * @param funExpr The function expression
     */
    public FunDeclGlobal(
            SourceLocation location,
            DeclAnnotation annotation,
            String name,
            int slot,
            FunExpr funExpr
    ) {
        super(location, annotation, name, slot, funExpr);
    }

    // ----- Execute methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Get the function value
        FunctionValue functionValue = this.funExpr.executeFunction(frame);
        functionValue.setName(this.name);
        functionValue.setMemoized(this.isMemoized);

        // Export the checker if needed
        if(this.checkerMode != CheckerMode.OFF) {
            this.exportChecker(frame, functionValue);
        }

        // Put the function value in the global context
        LKQLLanguage.getContext(this).setGlobal(this.slot, this.name, functionValue);

        // Return the unit value
        return UnitValue.getInstance();
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"name", "slot"},
                new Object[]{this.name, this.slot}
        );
    }

}
