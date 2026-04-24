//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.subcommands;

import com.adacore.lkql_jit.driver.checker.CheckerRun;
import picocli.CommandLine;

/**
 * This class represents the LKQL checker entry point with the LKQL JIT backend. This is a TEMPORARY
 * driver to perform efficiency tests on LKQL JIT in real life use case.
 */
@CommandLine.Command(
    name = "check",
    description = "Alternative checker driver. Like GNATcheck but with less options & a more " +
        "modern command line interface"
)
public class LKQLChecker extends BaseCheckerSubcommand {

    // ----- Constructors -----

    public LKQLChecker() {}

    // ----- Instance methods -----

    @Override
    protected CheckerRun.AutoFixMode getAutoFixMode() {
        return CheckerRun.AutoFixMode.NONE;
    }
}
