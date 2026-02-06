//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics.variants;

import java.util.Optional;

/** Diagnostic variant to simply forward a message without any additional information. */
public final class RawMessage extends BaseDiagnostic {

    public RawMessage(String message) {
        super(message, Optional.empty());
    }
}
