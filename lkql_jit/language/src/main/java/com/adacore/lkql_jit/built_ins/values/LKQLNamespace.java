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

package com.adacore.lkql_jit.built_ins.values;

import com.adacore.lkql_jit.built_ins.values.bases.ObjectLKQLValue;
import com.adacore.lkql_jit.built_ins.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.runtime.Cell;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.object.Shape;
import com.oracle.truffle.api.utilities.TriState;
import java.util.HashMap;
import java.util.Map;

/** This class represents the namespaces in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public class LKQLNamespace extends ObjectLKQLValue implements LKQLValue {

    // ----- Constructors -----

    /** Create a new LKQL namespace with its shape. */
    public LKQLNamespace(Shape shape) {
        super(shape);
    }

    // ----- Class methods -----

    /** Create a namespace from the given Truffle frame and its store values. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLNamespace createUncached(MaterializedFrame frame) {
        // Prepare the map for the symbols
        final Map<String, Object> symbols = new HashMap<>();

        // Get the frame descriptor and iterate on the frame slots to get the symbols
        final FrameDescriptor frameDescriptor = frame.getFrameDescriptor();
        for (int i = 0; i < frameDescriptor.getNumberOfSlots(); i++) {
            final String name = (String) frameDescriptor.getSlotName(i);
            if (name != null) {
                symbols.put(name, ((Cell) frame.getObject(i)).getRef());
            }
        }

        // Return the new namespace
        LKQLNamespace res = new LKQLNamespace(Shape.newBuilder().build());
        for (String key : symbols.keySet()) {
            uncachedObjectLibrary.put(res, key, symbols.get(key));
        }
        return res;
    }

    // ----- Value methods -----

    /** Exported message to compare two LKQL namespaces. */
    @ExportMessage
    static class IsIdenticalOrUndefined {
        /** Compare two LKQL namespaces. */
        @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
        protected static TriState onLKQLNamespace(
                final LKQLNamespace left,
                final LKQLNamespace right,
                @CachedLibrary("left") DynamicObjectLibrary lefts,
                @CachedLibrary("right") DynamicObjectLibrary rights,
                @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary leftValues,
                @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary rightValues) {
            return TriState.valueOf(
                    objectValueEquals(left, right, lefts, rights, leftValues, rightValues));
        }

        /** Do the comparison with another element. */
        @Fallback
        protected static TriState onOther(
                @SuppressWarnings("unused") final LKQLNamespace receiver,
                @SuppressWarnings("unused") final Object other) {
            return TriState.UNDEFINED;
        }
    }

    /** Get the identity hash code for the given namespace */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public static int identityHashCode(LKQLNamespace receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string for the interop library. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    Object toDisplayString(
            @SuppressWarnings("unused") final boolean allowSideEffects,
            @CachedLibrary("this") DynamicObjectLibrary thisLibrary,
            @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary elemLibrary) {
        // Prepare the result string builder and get the keys of the object
        StringBuilder resultBuilder = new StringBuilder("Namespace(");
        Object[] keys = thisLibrary.getKeyArray(this);

        // Iterate over keys and add the values
        for (int i = 0; i < keys.length; i++) {
            // Get key/value
            Object key = keys[i];
            Object value = thisLibrary.getOrDefault(this, key, null);

            // Create the string of the value
            String valueString;
            if (value instanceof String) {
                valueString = StringUtils.toRepr((String) elemLibrary.toDisplayString(value));
            } else {
                valueString = (String) elemLibrary.toDisplayString(value);
            }

            // Add the strings to the result
            resultBuilder.append(key).append(": ").append(valueString);
            if (i < keys.length - 1) resultBuilder.append(", ");
        }

        // Return the string result
        resultBuilder.append(")");
        return resultBuilder.toString();
    }
}
