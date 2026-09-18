//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics;

import com.adacore.lkql_jit.driver.diagnostics.variants.BaseDiagnostic;
import com.adacore.lkql_jit.driver.subcommands.BaseSubcommand;
import java.util.function.Consumer;

/** Base for all report creation classes. */
public abstract sealed class BaseReportCreator
    implements Consumer<BaseDiagnostic>
    permits SarifReportCreator, TextReportCreator {

    // ----- Attributes -----

    /** The minimal level of a diagnostics to be included in the final report. */
    protected final BaseSubcommand.VerbosityLevel verbosityLevel;

    // ----- Constructors -----

    protected BaseReportCreator(BaseSubcommand.VerbosityLevel verbosityLevel) {
        this.verbosityLevel = verbosityLevel;
    }
}
