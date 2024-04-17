//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes;

import com.adacore.lkql_jit.utils.source_location.Locatable;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;

/**
 * This class represents an identifier in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class Identifier implements Locatable {

    // ----- Attributes -----

    /** The location of the identifier in the source. */
    private final SourceLocation location;

    /** The name of the identifier. */
    private final String name;

    // ----- Constructors -----

    /**
     * Create a new identifier with the parameters.
     *
     * @param location The location of the identifier in the source.
     * @param name The name of the identifier.
     */
    public Identifier(SourceLocation location, String name) {
        this.location = location;
        this.name = name;
    }

    // ----- Getters -----

    /**
     * @see com.adacore.lkql_jit.utils.source_location.Locatable#getLocation()
     */
    @Override
    public SourceLocation getLocation() {
        return this.location;
    }

    public String getName() {
        return this.name;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return this.name;
    }
}
