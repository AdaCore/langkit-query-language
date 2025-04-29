//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.interfaces;

import com.adacore.lkql_jit.exception.utils.InvalidIndexException;

/**
 * This class defines the indexable interface for all the indexable LKQL types.
 *
 * @author Hugo GUERRIER
 */
public interface Indexable {
    /**
     * Get the element at the given index.
     *
     * @param index The index to get.
     * @return The element at the position.
     * @throws com.adacore.lkql_jit.exception.utils.InvalidIndexException If the index is not valid.
     */
    Object get(long index) throws InvalidIndexException;

    /**
     * Get the content of the indexable value in an array.
     *
     * @return The content of the indexable value.
     */
    Object[] getContent();
}
