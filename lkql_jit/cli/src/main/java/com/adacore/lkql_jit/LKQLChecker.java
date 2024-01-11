//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import picocli.CommandLine;

/**
 * This class represents the LKQL checker entry point with the LKQL JIT backend. This is a TEMPORARY
 * driver to perform efficiency tests on LKQL JIT in real life use case. TODO : Support all flags
 * and options of the lkql_checker original implementation.
 *
 * @author Hugo GUERRIER
 */
public class LKQLChecker extends BaseLKQLChecker {

    // ---- Subcommand spec -----

    /** This class defines the "check" LKQL subcommand. */
    @CommandLine.Command(
            name = "check",
            description =
                    "Alternative checker driver. Like GNATcheck but with less options "
                            + "& a more modern command line interface")
    public static class Args extends BaseLKQLChecker.Args {
        @Override
        public Integer call() {
            new LKQLChecker(this).launch(this.unmatched.toArray(new String[0]));
            return 0;
        }
    }

    // ----- Constructors -----

    public LKQLChecker(LKQLChecker.Args args) {
        super(args);
    }

    // ----- Instance methods -----

    @Override
    protected LKQLOptions getOptions() {
        return getBaseOptionsBuilder().engineMode(EngineMode.CHECKER).checkerDebug(true).build();
    }
}
