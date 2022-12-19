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
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.libadalang.Libadalang;


/**
 * This node represents the is clause in the LKQL language (i.e. node is AdaNode)
 *
 * @author Hugo GUERRIER
 */
@NodeChild(value = "nodeExpr", type = Expr.class)
public abstract class IsClause extends Expr {

    // ----- Attributes -----

    /** The location of the node expression */
    private final DummyLocation nodeLocation;

    // ----- Children -----

    /** The pattern node to evaluate the "is" clause */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new "is" clause with the parameters
     *
     * @param location The token location in the source
     * @param nodeLocation The location of the node expression node
     * @param pattern The pattern to execute the is clause
     */
    protected IsClause(
            SourceLocation location,
            DummyLocation nodeLocation,
            BasePattern pattern
    ) {
        super(location);
        this.nodeLocation = nodeLocation;
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /**
     * Execute the is clause when the expression is a node
     *
     * @param frame The frame to execute the pattern in
     * @param node The node to verify
     * @return The result of the pattern execution
     */
    @Specialization
    protected boolean executeNode(VirtualFrame frame, Libadalang.AdaNode node) {
        return this.pattern.executeNode(frame, node);
    }

    /**
     * Fallback method if the left operand is not a node
     *
     * @param notNode The object that is not a node
     */
    @Fallback
    protected void notNode(Object notNode) {
        throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.ADA_NODE,
                LKQLTypesHelper.fromJava(notNode),
                this.nodeLocation
        );
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
