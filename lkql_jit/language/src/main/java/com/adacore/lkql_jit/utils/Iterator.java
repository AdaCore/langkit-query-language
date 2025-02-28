//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils;

/**
 * This interface defines custom and generic iterators because the native one is not suitable for
 * JIT compilation.
 *
 * @author Hugo GUERRIER
 */
public interface Iterator {
    /**
     * Get if the iterator has a next element.
     *
     * @return True if there is a next element, false else.
     */
    boolean hasNext();

    /**
     * Get the next element by consuming the iterable.
     *
     * @return The next element.
     */
    Object next();

    /** Reset the iterator to the start. */
    void reset();
}
