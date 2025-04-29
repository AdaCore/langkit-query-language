//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.interfaces;

/**
 * This class represents all LKQL values that can be interpreted as a boolean.
 *
 * @author Hugo GUERRIER
 */
public interface Truthy {
    /**
     * Get the boolean representation of the object.
     *
     * @return True if the object is evaluated as true, false else.
     */
    boolean isTruthy();

    // ----- Class methods -----

    static Truthy wrapBoolean(boolean b) {
        return () -> b;
    }
}
