//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics.variants;

import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.util.Optional;

/** This class represents an error diagnostics. */
public final class Error extends BaseDiagnostic {

    // ----- Constructors -----

    /** Create a new error diagnostic with a related location. */
    public Error(String message, SourceSection location) {
        super(message, Optional.of(location));
    }

    /** Create a new error diagnostic without a location. */
    public Error(String message) {
        super(message, Optional.empty());
    }
}
