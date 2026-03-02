//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics;

import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.util.Optional;

/**
 * Represents a hint for a diagnostic. A hint is a (potentially located) additional information you
 * want to attach to a diagnostic.
 */
public final class Hint {

    // ----- Attributes -----

    public final String message;

    public final Optional<SourceSection> location;

    // ----- Constructors -----

    public Hint(String message) {
        this.message = message;
        this.location = Optional.empty();
    }

    public Hint(String message, SourceSection location) {
        this.message = message;
        this.location = Optional.of(location);
    }
}
