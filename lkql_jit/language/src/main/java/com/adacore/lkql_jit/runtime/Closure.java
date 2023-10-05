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

package com.adacore.lkql_jit.runtime;

import com.adacore.lkql_jit.utils.ClosureDescriptor;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.MaterializedFrame;
import java.util.Arrays;
import java.util.Map;

/**
 * This class represents a closure in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class Closure {

    // ----- Class attributes -----

    /** Singleton representing the empty closure. */
    public static final Closure EMPTY = new Closure(new Cell[0]);

    // ----- Instance attributes -----

    /** The content of the closure. */
    private final Cell[] content;

    // ----- Constructors -----

    /**
     * Create a new closure from its content.
     *
     * @param content The content of the closure.
     */
    public Closure(final Cell[] content) {
        this.content = content;
    }

    /**
     * Create a closure from a frame and the description of it.
     *
     * @param frame The frame to create the closure from.
     * @param closureDescriptor The description of the closure to create.
     * @return The newly created closure
     */
    @CompilerDirectives.TruffleBoundary
    public static Closure create(
            final MaterializedFrame frame, final ClosureDescriptor closureDescriptor) {
        // Create the content of the closure
        final Cell[] content = new Cell[closureDescriptor.getClosureSize()];

        // Put all needed locals in the closure
        for (Map.Entry<Integer, Integer> closingLocal :
                closureDescriptor.getClosingLocals().entrySet()) {
            content[closingLocal.getKey()] = (Cell) frame.getObject(closingLocal.getValue());
        }

        // Put all needed parameters in the closure
        for (Map.Entry<Integer, Integer> closingParameter :
                closureDescriptor.getClosingParameters().entrySet()) {
            content[closingParameter.getKey()] =
                    new Cell(frame.getArguments()[closingParameter.getValue()]);
        }

        // Put all needed closure values in the closure
        for (Map.Entry<Integer, Integer> closingClosure :
                closureDescriptor.getClosingClosures().entrySet()) {
            content[closingClosure.getKey()] =
                    ((Cell[]) frame.getArguments()[0])[closingClosure.getValue()];
        }

        // Return the new closure
        return new Closure(content);
    }

    // ----- Getters -----

    public Cell[] getContent() {
        return content;
    }

    // ----- Instance methods -----

    /**
     * Get the cell at the given slot.
     *
     * @param slot The slot to get the cell at.
     * @return The cell.
     */
    public Cell get(final int slot) {
        return this.content[slot];
    }

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        return "Closure(" + "content: " + Arrays.toString(this.content) + ")";
    }
}
