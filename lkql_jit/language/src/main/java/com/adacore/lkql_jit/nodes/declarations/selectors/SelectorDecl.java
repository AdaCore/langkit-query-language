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

import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.adacore.lkql_jit.nodes.declarations.DeclAnnotation;
import com.adacore.lkql_jit.nodes.declarations.Declaration;


/**
 * This node represents the base declaration of a selector in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public abstract class SelectorDecl extends Declaration {

    // ----- Attributes -----

    /** The name of the selector */
    protected final String name;

    /** The documentation of the selector */
    protected final String documentation;

    /** If the selector is memoized */
    protected final boolean isMemoized;

    /** The slot to put the selector in */
    protected final int slot;

    /** The slot for the "this" symbol */
    protected final int thisSlot;

    /** The slot for the "depth" symbol */
    protected final int depthSlot;

    /** The frame descriptor for the selector root node */
    protected final FrameDescriptor descriptor;

    // ----- Children -----

    /** The arms of the selector */
    @Children
    protected final SelectorArm[] arms;

    // ----- Constructors -----

    /**
     * Create a new selector declaration
     *
     * @param location The token location in the source
     * @param annotation The annotation of the selector declaration
     * @param name The name of the selector
     * @param documentation The documentation of the selector
     * @param slot The slot to put the selector in
     * @param thisSlot The slot for the "this" symbol
     * @param depthSlot The slot for the "depth" symbol
     * @param descriptor The frame descriptor for the selector
     * @param arms The arms of the selector
     */
    protected SelectorDecl(
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
        super(location);
        this.name = name;
        this.documentation = documentation;
        this.isMemoized = annotation != null && annotation.getName().equals(DeclAnnotation.MEMOIZED);
        this.slot = slot;
        this.thisSlot = thisSlot;
        this.depthSlot = depthSlot;
        this.descriptor = descriptor;
        this.arms = arms;
    }
    
}
