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

package com.adacore.lkql_jit.utils.functions;


import com.adacore.lkql_jit.runtime.Cell;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class contains all util functions about the frame managing.
 *
 * @author Hugo GUERRIER
 */
public final class FrameUtils {

    /**
     * Get the local value stored in the frame at the given slot.
     *
     * @param frame The frame to read the local in.
     * @param slot  The slot to read the local at.
     * @return The local value at the slot.
     */
    public static Object readLocal(
        final VirtualFrame frame,
        final int slot
    ) {
        return ((Cell) frame.getObject(slot)).getRef();
    }

    /**
     * Write the local value at the given slot in the given frame.
     *
     * @param frame The frame to write the value in.
     * @param slot  The slot to write the value at.
     * @param value The value to write.
     */
    public static void writeLocal(
        final VirtualFrame frame,
        final int slot,
        final Object value
    ) {
        ((Cell) frame.getObject(slot)).setRef(value);
    }

    /**
     * Read the closure value stored in the frame at the given slot.
     *
     * @param frame The frame to read the closure value in.
     * @param slot  The slot to read the closure at.
     * @return The closure value at the slot.
     */
    public static Object readClosure(
        final VirtualFrame frame,
        final int slot
    ) {
        return (((Cell[]) frame.getArguments()[0])[slot]).getRef();
    }

}
