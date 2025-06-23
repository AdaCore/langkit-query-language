//
//  Copyright (C) 2005-2025, AdaCore
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

    public enum DestKind {
        LOCAL,
        PARAM,
        CLOSURE,
    }

    public final int[] destinationSlots;

    public final DestKind[] destinationKinds;

    // ----- Constructors -----

    /**
     * Create a new closure descriptor with its values.
     */
    public ClosureDescriptor(
        final int closureSize,
        final Map<Integer, Integer> closingLocals,
        final Map<Integer, Integer> closingParameters,
        final Map<Integer, Integer> closingClosures
    ) {
        this.destinationSlots = new int[closureSize];
        this.destinationKinds = new DestKind[closureSize];

        for (var closingLocal : closingLocals.entrySet()) {
            destinationSlots[closingLocal.getKey()] = closingLocal.getValue();
            destinationKinds[closingLocal.getKey()] = DestKind.LOCAL;
        }
        for (var closingParam : closingParameters.entrySet()) {
            destinationSlots[closingParam.getKey()] = closingParam.getValue();
            destinationKinds[closingParam.getKey()] = DestKind.PARAM;
        }
        for (var closingClosure : closingClosures.entrySet()) {
            destinationSlots[closingClosure.getKey()] = closingClosure.getValue();
            destinationKinds[closingClosure.getKey()] = DestKind.CLOSURE;
        }
    }
}
