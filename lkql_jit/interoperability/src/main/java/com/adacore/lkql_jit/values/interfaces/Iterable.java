//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interfaces;

/**
 * This interface defines the iterable interface for all the iterable LKQL types.
 *
 * @author Hugo GUERRIER
 */
public interface Iterable {
    /**
     * Get the iterator for the iterable object.
     *
     * @return The iterator.
     */
    Iterator iterator();
}
