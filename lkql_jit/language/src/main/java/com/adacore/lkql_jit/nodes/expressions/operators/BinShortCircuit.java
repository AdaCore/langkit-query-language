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

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.UnexpectedResultException;


/**
 * This class represent the base of the binary operation node that are edibles to the short circuit strategy
 *
 * @author Hugo GUERRIER
 */
public abstract class BinShortCircuit extends Expr {

    // ----- Children -----

    /** The left operand expression */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected Expr left;

    /** The right operand expression */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected Expr right;

    // ----- Constructors -----

    /**
     * Create a new bin op short circuit node
     *
     * @param location The location of the node in the source
     * @param left The left expression
     * @param right The right expression
     */
    protected BinShortCircuit(
            SourceLocation location,
            Expr left,
            Expr right
    ) {
        super(location);
        this.left = left;
        this.right = right;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeBoolean(frame);
    }

    /** @see com.adacore.lkql_jit.nodes.expressions.Expr#executeBoolean(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public boolean executeBoolean(VirtualFrame frame) {
        // Execute the left value
        boolean leftValue;
        try {
            leftValue = this.left.executeBoolean(frame);
        } catch (UnexpectedResultException e) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_BOOLEAN,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.left
            );
        }

        // Execute the right value if needed
        boolean rightValue = false;
        if(this.doRightEvaluation(leftValue)) {
            try {
                rightValue =  this.right.executeBoolean(frame);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_BOOLEAN,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.right
                );
            }
        }

        // Return the result
        return this.execute(leftValue, rightValue);
    }

    // ----- Class methods -----

    /**
     * Get if the right execution is necessary
     *
     * @param leftValue The left value
     * @return True if the right execution is necessary, false else
     */
    protected abstract boolean doRightEvaluation(boolean leftValue);

    /**
     * Do the execution for the left and right value
     *
     * @param leftValue The left value
     * @param rightValue The right value
     * @return The result of the node execution
     */
    protected abstract boolean execute(boolean leftValue, boolean rightValue);

}
