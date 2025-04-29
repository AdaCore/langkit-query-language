//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

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
        final MaterializedFrame frame,
        final ClosureDescriptor closureDescriptor
    ) {
        // Create the content of the closure
        final Cell[] content = new Cell[closureDescriptor.getClosureSize()];

        // Put all needed locals in the closure
        for (Map.Entry<Integer, Integer> closingLocal : closureDescriptor
            .getClosingLocals()
            .entrySet()) {
            content[closingLocal.getKey()] = (Cell) frame.getObject(closingLocal.getValue());
        }

        // Put all needed parameters in the closure
        for (Map.Entry<Integer, Integer> closingParameter : closureDescriptor
            .getClosingParameters()
            .entrySet()) {
            content[closingParameter.getKey()] = new Cell(
                frame.getArguments()[closingParameter.getValue()]
            );
        }

        // Put all needed closure values in the closure
        for (Map.Entry<Integer, Integer> closingClosure : closureDescriptor
            .getClosingClosures()
            .entrySet()) {
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

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        return "Closure(" + "content: " + Arrays.toString(this.content) + ")";
    }
}
