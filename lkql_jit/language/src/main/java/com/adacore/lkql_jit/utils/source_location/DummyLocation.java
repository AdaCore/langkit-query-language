//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.source_location;

/**
 * This class is a dummy location, a locatable when nodes aren't reachable.
 *
 * @author Hugo GUERRIER
 */
public final class DummyLocation implements Locatable {

    // ----- Attributes -----

    /** The represented location. */
    private final SourceLocation location;

    // ----- Constructors -----

    /**
     * Create a new dummy location.
     *
     * @param location The location of the dummy.
     */
    public DummyLocation(SourceLocation location) {
        this.location = location;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.utils.source_location.Locatable#getLocation()
     */
    @Override
    public SourceLocation getLocation() {
        return this.location;
    }
}
