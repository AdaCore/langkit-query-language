//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils;

import java.util.Map;

/**
 * This class represents the description of a closure.
 *
 * @author Hugo GUERRIER
 */
public final class ClosureDescriptor {

    // ----- Attributes -----

    /** Size of the closure. */
    private final int closureSize;

    /** Map that goes from closure slots to local slots to close. */
    private final Map<Integer, Integer> closingLocals;

    /** Map that goes from closure slots to parameters slots to close. */
    private final Map<Integer, Integer> closingParameters;

    /** Map that goes from closure slots to upper closure slots to close. */
    private final Map<Integer, Integer> closingClosures;

    // ----- Constructors -----

    /**
     * Create a new closure descriptor with its values.
     *
     * @param closureSize Size of the closure.
     * @param closingLocals Local values to enclose.
     * @param closingParameters Parameters to enclose.
     * @param closingClosures Closure values to enclose.
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
