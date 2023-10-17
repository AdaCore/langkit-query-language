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

package com.adacore.lkql_jit.nodes.expressions.match;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.UnitValue;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;

/**
 * This node represents a pattern matching expression in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class Match extends Expr {

    // ----- Children -----

    /** Expression to match. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    /** Matching arms. */
    @Children private final MatchArm[] arms;

    // ----- constructors -----

    /**
     * Create a new match node.
     *
     * @param location The location of the node in the source.
     * @param expr The expression to match.
     * @param arms The matching arms.
     */
    public Match(SourceLocation location, Expr expr, MatchArm[] arms) {
        super(location);
        this.expr = expr;
        this.arms = arms;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        // Evaluate the expression as a node
        Object toMatch = this.expr.executeGeneric(frame);

        // For every match arm try to match and return the result
        for (MatchArm arm : this.arms) {
            Object armResult = arm.executeArm(frame, toMatch);
            if (armResult != null) {
                return armResult;
            }
        }

        // If no arm matched, return the unit value
        // Probably want to raise an exception in the future
        return UnitValue.getInstance();
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
