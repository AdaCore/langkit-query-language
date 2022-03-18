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
import com.adacore.lkql_jit.LKQLTypeSystemGen;


/**
 * This node represents the "unwrap" operation in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class Unwrap extends Expr {

    // ----- Children -----

    /** The expression of the node to unwrap */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr nodeExpr;

    // ----- Constructors -----

    /**
     * Create a new unwrap node with parameters
     *
     * @param location The location of the node in the source
     * @param nodeExpr The node expression
     */
    public Unwrap(SourceLocation location, Expr nodeExpr) {
        super(location);
        this.nodeExpr = nodeExpr;
    }

    // ----- Execution methods -----
    
    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Evaluate the node value and test it
        Object nodeValue = this.nodeExpr.executeGeneric(frame);
        if(!LKQLTypeSystemGen.isAdaNode(nodeValue)) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.ADA_NODE,
                    LKQLTypesHelper.fromJava(nodeValue),
                    this.nodeExpr
            );
        }

        // Return the node value
        return nodeValue;
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
