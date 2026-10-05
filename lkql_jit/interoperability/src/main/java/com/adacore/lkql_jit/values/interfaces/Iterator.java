//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interfaces;

/**
 * Custom interface for iterator objects. We don't use the Java one for Truffle compilation
 * concerns.
 */
public interface Iterator {
    /** Get whether the iterator has a next element. */
    boolean hasNext();

    /** Get the next element by consuming the iterator. */
    Object next();

    /** Reset the iterator to the start. */
    void reset();
}
