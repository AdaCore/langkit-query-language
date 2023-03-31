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

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;


/**
 * This node represents the conditional branching expression in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class IfThenElse extends Expr {

    // ----- Children -----

    /**
     * The condition of the branching
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr condition;

    /**
     * The consequence of the branching
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr consequence;

    /**
     * The alternative of the branching
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr alternative;

    // ----- Constructors -----

    /**
     * Create a new if then else node
     *
     * @param location    The location of the node in the source
     * @param condition   The condition expression
     * @param consequence The consequence expression
     * @param alternative The alternative expression
     */
    public IfThenElse(
        SourceLocation location,
        Expr condition,
        Expr consequence,
        Expr alternative
    ) {
        super(location);
        this.condition = condition;
        this.consequence = consequence;
        this.alternative = alternative;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Evaluate the condition as a boolean
        boolean conditionValue;
        try {
            conditionValue = this.condition.executeBoolean(frame);
        } catch (UnexpectedResultException e) {
            throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_BOOLEAN,
                LKQLTypesHelper.fromJava(e.getResult()),
                this.condition
            );
        }

        // Execute the correct branching
        if (conditionValue) {
            return this.consequence.executeGeneric(frame);
        } else {
            return this.alternative.executeGeneric(frame);
        }
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
