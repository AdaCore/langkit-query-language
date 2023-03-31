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

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents the base for all patterns in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public abstract class BasePattern extends LKQLNode {

    /**
     * Create a new base pattern node
     *
     * @param location The location of the node in the source
     */
    protected BasePattern(
        SourceLocation location
    ) {
        super(location);
    }

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public final Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the pattern and get if the node fulfills it
     *
     * @param frame The frame to execute the pattern in
     * @param node  The node to verify
     * @return True of the node verify the pattern, false else
     */
    public abstract boolean executeNode(VirtualFrame frame, Libadalang.AdaNode node);

    /**
     * Execute the pattern on a string
     *
     * @param frame The frame to execute in
     * @param str   The string to verify
     * @return If the string fulfills the pattern
     */
    public boolean executeString(VirtualFrame frame, String str) {
        throw LKQLRuntimeException.wrongType(
            LKQLTypesHelper.ADA_NODE,
            LKQLTypesHelper.LKQL_STRING,
            this
        );
    }

}
