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

package com.adacore.lkql_jit.nodes.arguments;

import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents all type of arguments in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class Arg extends LKQLNode {

    // ----- Attributes -----

    /** The argument name, can be null if the arg is an expr one. */
    protected final Identifier argName;

    // ----- Children -----

    /** The expression of the argument. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected Expr argExpr;

    // ----- Constructors -----

    /**
     * Create a new argument node.
     *
     * @param location The location of the argument in the sources.
     * @param argName The name of the argument.
     * @param argExpr The expression of the argument.
     */
    protected Arg(SourceLocation location, Identifier argName, Expr argExpr) {
        super(location);
        this.argName = argName;
        this.argExpr = argExpr;
    }

    // ----- Getters -----

    public Identifier getArgName() {
        return this.argName;
    }

    public Expr getArgExpr() {
        return this.argExpr;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.argExpr.executeGeneric(frame);
    }
}
