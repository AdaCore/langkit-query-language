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

package com.adacore.lkql_jit.nodes.declarations;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents the declaration of a parameter in a function signature in LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ParameterDeclaration extends Declaration {

    // ----- Attributes -----

    /** Parameter's name. */
    private final String name;

    /** Parameter's default value. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr defaultValue;

    // ----- Constructors -----

    /**
     * Create a new parameter declaration node.
     *
     * @param location The location of the node in the source.
     * @param name The name of the parameter.
     * @param defaultValue The default value of the parameter (can be null).
     */
    public ParameterDeclaration(
            final SourceLocation location, final String name, final Expr defaultValue) {
        super(location, null);
        this.name = name;
        this.defaultValue = defaultValue;
    }

    // ----- Getters -----

    public String getName() {
        return this.name;
    }

    public Expr getDefaultValue() {
        return this.defaultValue;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Fail because this node is not executable as a generic one
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"name"}, new Object[] {this.name});
    }
}
