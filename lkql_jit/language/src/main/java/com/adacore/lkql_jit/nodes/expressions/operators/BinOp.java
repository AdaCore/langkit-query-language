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
import com.adacore.lkql_jit.utils.SourceLocation;
import com.oracle.truffle.api.dsl.NodeChild;


/**
 * This node is the base of all binary operations in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@NodeChild(value = "left", type = Expr.class)
@NodeChild(value = "right", type = Expr.class)
public abstract class BinOp extends Expr {

    // ----- Attributes -----

    /**
     * The location of the left node.
     */
    protected final SourceLocation leftLocation;

    /**
     * The location of the right node.
     */
    protected final SourceLocation rightLocation;

    // ----- Constructors -----

    /**
     * Create a binary operation node.
     *
     * @param location      The location of the node in the source.
     * @param leftLocation  The location of the left node.
     * @param rightLocation The location of the right node.
     */
    protected BinOp(
        SourceLocation location,
        SourceLocation leftLocation,
        SourceLocation rightLocation
    ) {
        super(location);
        this.leftLocation = leftLocation;
        this.rightLocation = rightLocation;
    }

}
