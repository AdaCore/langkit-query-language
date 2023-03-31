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

package com.adacore.lkql_jit.utils.util_classes;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.MaterializedFrame;


/**
 * This class represents the closure concepts in the LKQL JIT
 *
 * @author Hugo GUERRIER
 */
public final class Closure {

    // ----- Attributes -----

    /**
     * The content of the closure
     */
    private final Object[] content;

    /**
     * The limit of the closure
     */
    private final int limit;

    // ----- Constructors -----

    /**
     * Create a new closure
     *
     * @param frame The frame to create the closure from
     */
    public Closure(
        MaterializedFrame frame,
        int limit
    ) {
        this.limit = limit;
        this.content = new Object[Math.min(frame.getFrameDescriptor().getNumberOfSlots(), this.limit)];
        for (int i = 0; i < this.content.length; i++) {
            this.content[i] = frame.getObject(i);
        }
    }

    // ----- Class methods -----

    /**
     * Instantiate the closure in the given frame
     *
     * @param frame The frame to instantiate the closure in
     */
    @CompilerDirectives.TruffleBoundary
    public void instantiate(MaterializedFrame frame) {
        for (int i = 0; i < this.content.length; i++) {
            if (this.content[i] != null) {
                frame.setObject(i, this.content[i]);
            }
        }
    }

    /**
     * Set the object in the closure
     *
     * @param index The index of the object to set
     * @param value The value to place in the closure
     */
    @CompilerDirectives.TruffleBoundary
    public void setObject(int index, Object value) {
        if (index >= 0 && index < this.content.length) {
            this.content[index] = value;
        } else {
            throw new IndexOutOfBoundsException(index);
        }
    }

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        StringBuilder res = new StringBuilder("Closure{");
        res.append("limit=").append(this.limit).append(" | ");
        for (int i = 0; i < this.content.length; i++) {
            res.append(i).append(": ").append(
                this.content[i] == null ?
                    "null" :
                    this.content[i].toString()
            );
            if (i < this.content.length - 1) res.append(", ");
        }
        res.append("}");
        return res.toString();
    }

}
