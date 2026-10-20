//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interfaces;

/** This class defines the indexable interface for all the indexable LKQL types. */
public interface Indexable {
    /**
     * Get the element at the given index.
     *
     * @throws IndexOutOfBoundsException If the index is not valid.
     */
    Object get(long index) throws IndexOutOfBoundsException;
}
