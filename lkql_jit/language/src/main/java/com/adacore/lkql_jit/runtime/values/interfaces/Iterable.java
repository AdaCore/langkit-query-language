//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.interfaces;

import com.adacore.lkql_jit.utils.Iterator;

/**
 * This interface defines the iterable interface for all the iterable LKQL types.
 *
 * @author Hugo GUERRIER
 */
public interface Iterable {

    /**
     * Get the size of the iterable collection.
     *
     * @return The size of the iterable.
     */
    long size();

    /**
     * Get the iterator for the iterable object.
     *
     * @return The iterator.
     */
    Iterator iterator();
}
