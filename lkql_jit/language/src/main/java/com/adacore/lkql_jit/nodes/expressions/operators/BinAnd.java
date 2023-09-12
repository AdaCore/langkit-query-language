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

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;


/**
 * This node represent the logic "and" operation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class BinAnd extends BinShortCircuit {

    // ----- Constructors -----

    /**
     * Create an "and" node.
     *
     * @param location The location of the node in the source.
     * @param left     The left expression.
     * @param right    The right expression.
     */
    public BinAnd(
        SourceLocation location,
        Expr left,
        Expr right
    ) {
        super(location, left, right);
    }


    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.expressions.operators.BinShortCircuit#doRightEvaluation(boolean)
     */
    @Override
    protected boolean doRightEvaluation(boolean leftValue) {
        return leftValue;
    }

    /**
     * @see com.adacore.lkql_jit.nodes.expressions.operators.BinShortCircuit#execute(boolean, boolean)
     */
    @Override
    protected boolean execute(boolean leftValue, boolean rightValue) {
        return leftValue && rightValue;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
