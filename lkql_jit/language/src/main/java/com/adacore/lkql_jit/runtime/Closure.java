//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.Arrays;

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
