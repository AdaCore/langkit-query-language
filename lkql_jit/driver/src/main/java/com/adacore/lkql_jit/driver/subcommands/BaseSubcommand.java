//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.subcommands;

import org.graalvm.launcher.AbstractLanguageLauncher;

public abstract class BaseSubcommand extends AbstractLanguageLauncher {

    // ----- Attributes -----

    /** Whether the current output support ANSI colors. */
    protected final boolean supportAnsi;

    // ----- Constructors -----

    protected BaseSubcommand() {
        this.supportAnsi = System.getenv("TERM") != null && System.console() != null;
    }
}
