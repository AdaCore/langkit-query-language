//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.values.interfaces;

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
}
