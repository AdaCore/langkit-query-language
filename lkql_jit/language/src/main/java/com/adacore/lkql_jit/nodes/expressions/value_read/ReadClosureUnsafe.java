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

package com.adacore.lkql_jit.nodes.expressions.value_read;


import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents a value reading in the closure when we don't know if the value is defined.
 *
 * @author Hugo GUERRIER
 */
public final class ReadClosureUnsafe extends BaseRead {

    // ----- Attributes -----

    /**
     * Symbol to read in the closure.
     */
    private final String name;

    // ----- Constructors -----

    /**
     * Create a new closure read unsafe node.
     *
     * @param location The location of the node in the source.
     * @param slot     The closure slot to read.
     * @param name     The name of the closure symbol.
     */
    public ReadClosureUnsafe(
        final SourceLocation location,
        final int slot,
        final String name
    ) {
        super(location, slot);
        this.name = name;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        final Object res = FrameUtils.readClosure(frame, this.slot);
        if (res == null) {
            throw LKQLRuntimeException.unknownSymbol(name, this);
        } else {
            return res;
        }
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
            indentLevel,
            new String[]{"slot"},
            new Object[]{this.slot}
        );
    }

}
