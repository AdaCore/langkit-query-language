//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.enums;

/**
 * This enum represents the output mode for emitted diagnostics.
 *
 * @author Hugo GUERRIER
 */
public enum DiagnosticOutputMode {
    /**
     * Emit a pretty diagnostic with source listing where the diagnostic location is highlighted.
     */
    PRETTY,

    /** Use a GNATCheck-compliant format: "{file}:{line}:{col} check: {message} [{check}]". */
    GNATCHECK,
}
