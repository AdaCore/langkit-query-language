//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.source_location;

/**
 * This interface defines the locatable interface, a locatable object has a location in the LKQL
 * source.
 *
 * @author Hugo GUERRIER
 */
public interface Locatable {

    /**
     * Get the location of the object in the LKQL source.
     *
     * @return The object location in the source.
     */
    SourceLocation getLocation();
}
