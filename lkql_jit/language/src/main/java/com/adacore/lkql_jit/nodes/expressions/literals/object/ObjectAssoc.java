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

package com.adacore.lkql_jit.nodes.expressions.literals.object;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents an association in an object literal
 *
 * @author Hugo GUERRIER
 */
public final class ObjectAssoc extends LKQLNode {

    // ----- Attributes -----

    /**
     * The key of the association
     */
    private final String key;

    // ----- Children -----

    /**
     * The value of the association
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr value;

    // ----- Constructors -----

    /**
     * Create a new object association node
     *
     * @param location The location of the node in the source
     * @param key      The key of the association
     * @param value    The value of the association
     */
    public ObjectAssoc(
        SourceLocation location,
        String key,
        Expr value
    ) {
        super(location);
        this.key = key;
        this.value = value;
    }

    // ----- Getters -----

    public String getKey() {
        return key;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the value expression of the association
     *
     * @param frame The frame to execute in
     * @return The result of the value execution
     */
    public Object executeAssoc(VirtualFrame frame) {
        return this.value.executeGeneric(frame);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return null;
    }

}
