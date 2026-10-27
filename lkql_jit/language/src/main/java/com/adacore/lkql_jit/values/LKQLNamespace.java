//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.runtime.Cell;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.values.interop.LKQLBaseNamespace;
import com.adacore.lkql_jit.values.interop.LKQLList;
import com.adacore.lkql_jit.values.lists.LKQLArrayList;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached.Exclusive;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.object.Shape;
import com.oracle.truffle.api.source.SourceSection;
import java.util.Set;

/** This class represents the namespace of a module. */
@ExportLibrary(InteropLibrary.class)
public class LKQLNamespace extends LKQLBaseNamespace {

    // ----- Constructors -----

    /** Create a new LKQL namespace with its shape. */
    public LKQLNamespace(Shape shape, SourceSection creationLocation, String documentation) {
        super(shape, creationLocation, documentation);
    }

    // ----- Class methods -----

    /**
     * Create a namespace from the given Truffle frame and its store values.
     *
     * @param optInSymbols Include in the created namespace only symbols that are contained in
     *                     this set.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLNamespace createUncached(
        MaterializedFrame frame,
        Set<String> optInSymbols,
        String doc,
        SourceSection creationLocation
    ) {
        // Prepare the map for the symbols
        var res = new LKQLNamespace(Shape.newBuilder().build(), creationLocation, doc);

        // Get the frame descriptor and iterate on the frame slots to get the symbols
        final FrameDescriptor frameDescriptor = frame.getFrameDescriptor();
        for (int i = 0; i < frameDescriptor.getNumberOfSlots(); i++) {
            final String name = (String) frameDescriptor.getSlotName(i);
            if (name != null && optInSymbols.contains(name)) {
                uncachedObjectLibrary.put(res, name, ((Cell) frame.getObject(i)).getRef());
            }
        }
        return res;
    }

    // ----- Instance methods -----

    @Override
    protected LKQLList getKeys(DynamicObjectLibrary lib) {
        return new LKQLArrayList(lib.getKeyArray(this));
    }

    // ----- Value methods -----

    /** Get the displayable string for the interop library. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    Object toDisplayString(
        @SuppressWarnings("unused") final boolean allowSideEffects,
        @CachedLibrary("this") DynamicObjectLibrary thisLibrary,
        @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) @Exclusive InteropLibrary elemLibrary
    ) {
        // Prepare the result string builder and get the keys of the object
        StringBuilder resultBuilder = new StringBuilder("Namespace(");
        Object[] keys = thisLibrary.getKeyArray(this);

        // Iterate over keys and add the values
        for (int i = 0; i < keys.length; i++) {
            // Get key/value
            Object key = keys[i];
            Object value = thisLibrary.getOrDefault(this, key, null);

            resultBuilder.append(key);
            // TODO: ideally value should never be null (#643)
            if (value != null) {
                // Create the string of the value
                String valueString;
                if (value instanceof String) {
                    valueString = StringUtils.toRepr((String) elemLibrary.toDisplayString(value));
                } else {
                    valueString = (String) elemLibrary.toDisplayString(value);
                }

                // Add the strings to the result
                resultBuilder.append(": ").append(valueString);
            }

            if (i < keys.length - 1) resultBuilder.append(", ");
        }

        // Return the string result
        resultBuilder.append(")");
        return resultBuilder.toString();
    }

    // ----- Override methods -----

    @Override
    public boolean equals(Object o) {
        if (o instanceof LKQLNamespace) return super.equals(o);
        else return false;
    }
}
