//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics.variants;

import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.util.Optional;

/** This class represents a warning diagnostic. */
public final class Warning extends BaseDiagnostic {

    // ----- Constructors -----

    /** Create a new warning diagnostic with a related location. */
    public Warning(String message, SourceSection location) {
        super(message, Optional.ofNullable(location));
    }

    /** Create a new warning diagnostic without location. */
    public Warning(String message) {
        this(message, null);
    }
}
