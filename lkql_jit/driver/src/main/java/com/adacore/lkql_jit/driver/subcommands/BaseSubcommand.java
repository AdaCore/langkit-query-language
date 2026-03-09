//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.subcommands;

import com.adacore.lkql_jit.driver.diagnostics.DiagnosticCollector;
import com.adacore.lkql_jit.driver.diagnostics.DiagnosticLogHandler;
import com.adacore.lkql_jit.driver.source_support.SourceLinesCache;
import org.graalvm.launcher.AbstractLanguageLauncher;

public abstract class BaseSubcommand extends AbstractLanguageLauncher {

    // ----- Attributes -----

    /** Whether the current output support ANSI colors. */
    protected final boolean supportAnsi;

    /** Provide support for diagnostic collection in sub-commands. */
    protected final DiagnosticCollector diagnostics;

    /** Handle log from the Truffle engine. */
    protected final DiagnosticLogHandler logHandler;

    /** Object used to cache sources lines from files when fetching their content. */
    protected final SourceLinesCache linesCache;

    // ----- Constructors -----

    protected BaseSubcommand() {
        this.supportAnsi = System.getenv("TERM") != null && System.console() != null;
        this.diagnostics = new DiagnosticCollector();
        this.linesCache = new SourceLinesCache();
        this.logHandler = new DiagnosticLogHandler(diagnostics, linesCache);
    }
}
