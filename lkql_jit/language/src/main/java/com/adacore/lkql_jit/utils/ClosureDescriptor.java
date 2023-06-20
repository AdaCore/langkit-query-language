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

package com.adacore.lkql_jit.utils;


import java.util.Map;

/**
 * This class represents the description of a closure.
 *
 * @author Hugo GUERRIER
 */
public final class ClosureDescriptor {

    // ----- Attributes -----

    /**
     * Size of the closure.
     */
    private final int closureSize;

    /**
     * Map that goes from closure slots to local slots to close.
     */
    private final Map<Integer, Integer> closingLocals;

    /**
     * Map that goes from closure slots to parameters slots to close.
     */
    private final Map<Integer, Integer> closingParameters;

    /**
     * Map that goes from closure slots to upper closure slots to close.
     */
    private final Map<Integer, Integer> closingClosures;

    // ----- Constructors -----

    /**
     * Create a new closure descriptor with its values.
     *
     * @param closureSize       Size of the closure.
     * @param closingLocals     Local values to enclose.
     * @param closingParameters Parameters to enclose.
     * @param closingClosures   Closure values to enclose.
     */
    public ClosureDescriptor(
        final int closureSize,
        final Map<Integer, Integer> closingLocals,
        final Map<Integer, Integer> closingParameters,
        final Map<Integer, Integer> closingClosures
    ) {
        this.closureSize = closureSize;
        this.closingLocals = closingLocals;
        this.closingParameters = closingParameters;
        this.closingClosures = closingClosures;
    }

    // ----- Getters -----

    public int getClosureSize() {
        return this.closureSize;
    }

    public Map<Integer, Integer> getClosingLocals() {
        return this.closingLocals;
    }

    public Map<Integer, Integer> getClosingParameters() {
        return this.closingParameters;
    }

    public Map<Integer, Integer> getClosingClosures() {
        return this.closingClosures;
    }

}
