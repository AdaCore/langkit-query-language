//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics.variants;

import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.util.Optional;

/** This class represents an information diagnostic. */
public final class Info extends BaseDiagnostic {

    /** Create a new information diagnostic with a location. */
    public Info(String message, SourceSection location) {
        super(message, Optional.ofNullable(location));
    }

    /** Create a new information diagnostic without location. */
    public Info(String message) {
        this(message, null);
    }
}
