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

package com.adacore.lkql_jit.nodes.declarations.selectors;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.util_classes.Closure;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.lkql_jit.nodes.declarations.DeclAnnotation;
import com.adacore.lkql_jit.runtime.values.SelectorValue;
import com.adacore.lkql_jit.runtime.values.UnitValue;


/**
 * This class represents a global selector declaration in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class SelectorDeclGlobal extends SelectorDecl {

    // ----- Constructors ------

    /**
     * Create a new selector declaration in the global scope
     *
     * @param location The location of the node in the source
     * @param annotation The annotation of the declaration
     * @param name The name of the selector
     * @param documentation The documentation of the selector
     * @param slot The slot to put the selector in
     * @param thisSlot The slot for the "this" symbol
     * @param depthSlot The slot for the "depth" symbol
     * @param descriptor The descriptor for the selector
     * @param arms The arms of the selector
     */
    public SelectorDeclGlobal(
            SourceLocation location,
            DeclAnnotation annotation,
            String name,
            String documentation,
            int slot,
            int thisSlot,
            int depthSlot,
            FrameDescriptor descriptor,
            SelectorArm[] arms
    ) {
        super(location, annotation, name, documentation, slot, thisSlot, depthSlot, descriptor, arms);
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Create the selector value
        SelectorValue selectorValue = new SelectorValue(
                this.descriptor,
                null, // TODO : verify the validity of the null closure
                this.isMemoized,
                this.name,
                this.documentation,
                this.thisSlot,
                this.depthSlot,
                this.arms
        );

        // Put the value in the local context
        LKQLLanguage.getContext(this).setGlobal(this.slot, this.name, selectorValue);

        // Return the unit
        return UnitValue.getInstance();
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"name", "slot"},
                new Object[]{this.name, this.slot}
        );
    }

}
