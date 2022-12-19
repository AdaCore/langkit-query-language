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

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.libadalang.Libadalang;


/**
 * This node represents a binding pattern in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class BindingPattern extends UnfilteredPattern {

    // ----- Macros and enums -----

    /** The mode of binding */
    public enum Mode {
        LOCAL,
        GLOBAL
    }

    // ----- Attributes -----

    /** The mode of the binding */
    private final Mode mode;

    /** The slot to put the node in */
    private final int slot;

    // ----- Children -----

    /** The pattern to execute with the binding done */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private ValuePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new binding pattern node
     *
     * @param location The location of the node in the source
     * @param mode The binding mode
     * @param slot The slot to put the node in
     * @param pattern The pattern to bind
     */
    public BindingPattern(
            SourceLocation location,
            Mode mode,
            int slot,
            ValuePattern pattern
    ) {
        super(location);
        this.mode = mode;
        this.slot = slot;
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.patterns.BasePattern#executeNode(com.oracle.truffle.api.frame.VirtualFrame, com.adacore.libadalang.Libadalang.AdaNode) */
    @Override
    public boolean executeNode(VirtualFrame frame, Libadalang.AdaNode node) {
        // Do the node binding
        if(mode == Mode.GLOBAL) {
            LKQLLanguage.getContext(this).setGlobal(this.slot, null, node);
        } else {
            frame.setObject(this.slot, node);
        }

        // Execute the pattern with the binding done
        return this.pattern.executeNode(frame, node);
    }

    /** @see com.adacore.lkql_jit.nodes.patterns.BasePattern#executeString(com.oracle.truffle.api.frame.VirtualFrame, String) */
    @Override
    public boolean executeString(VirtualFrame frame, String str) {
        // Do the node binding
        if(mode == Mode.GLOBAL) {
            LKQLLanguage.getContext(this).setGlobal(this.slot, null, str);
        } else {
            frame.setObject(this.slot, str);
        }

        // Execute the pattern with the binding done
        return this.pattern.executeString(frame, str);
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"mode", "slot"},
                new Object[]{this.mode, this.slot}
        );
    }

}
