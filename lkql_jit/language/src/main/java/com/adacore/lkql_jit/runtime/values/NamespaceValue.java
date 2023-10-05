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

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.runtime.Cell;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.MaterializedFrame;
import java.util.HashMap;
import java.util.Map;

/**
 * This class represents the namespace values in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class NamespaceValue implements LKQLValue {

    // ----- Attributes -----

    /** Map which contains the symbols in the namespace. */
    private final Map<String, Object> symbols;

    // ----- Constructors -----

    /**
     * Create a new namespace with its name and bindings.
     *
     * @param symbols The symbols in the namespace.
     */
    public NamespaceValue(final Map<String, Object> symbols) {
        this.symbols = symbols;
    }

    /**
     * Create a namespace from the given frame.
     *
     * @param frame The frame to create the namespace from.
     * @return The newly created namespace.
     */
    @CompilerDirectives.TruffleBoundary
    public static NamespaceValue create(final MaterializedFrame frame) {
        // Prepare the map for the symbols
        final Map<String, Object> symbols = new HashMap<>();

        // Get the frame descriptor to iterate on the frame slots
        final FrameDescriptor frameDescriptor = frame.getFrameDescriptor();
        for (int i = 0; i < frameDescriptor.getNumberOfSlots(); i++) {
            final String name = (String) frameDescriptor.getSlotName(i);
            if (name != null) {
                symbols.put(name, ((Cell) frame.getObject(i)).getRef());
            }
        }

        // Return the new namespace
        return new NamespaceValue(symbols);
    }

    // ----- Getters -----

    /**
     * Get a value from the namespace with its name.
     *
     * @param symbol The name of the value to get.
     * @return The value if it exists, null else.
     */
    @CompilerDirectives.TruffleBoundary
    public Object get(final String symbol) {
        return this.symbols.getOrDefault(symbol, null);
    }

    // ----- Value methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue)
     */
    @Override
    @CompilerDirectives.TruffleBoundary
    public boolean internalEquals(LKQLValue o) {
        if (o == this) return true;
        if (!(o instanceof NamespaceValue other)) return false;
        return this.symbols.equals(other.symbols);
    }

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        // Return the string
        return "Namespace <symbols = " + this.symbols + ">";
    }
}
