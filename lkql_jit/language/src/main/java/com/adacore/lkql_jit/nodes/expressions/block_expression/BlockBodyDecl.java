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

package com.adacore.lkql_jit.nodes.expressions.block_expression;

import com.adacore.lkql_jit.nodes.declarations.Declaration;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents a declaration part of a block expression
 *
 * @author Hugo GUERRIER
 */
public final class BlockBodyDecl extends BlockBody {

    // ----- Children -----

    /**
     * The declaration of the body part
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Declaration decl;

    // ----- Constructors -----

    /**
     * Create a new block body declaration part
     *
     * @param location The location of the node in the source
     * @param decl     The declaration of the body part
     */
    public BlockBodyDecl(
        SourceLocation location,
        Declaration decl
    ) {
        super(location);
        this.decl = decl;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBody#executeBlockBody(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeBlockBody(VirtualFrame frame) {
        return this.decl.executeGeneric(frame);
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
