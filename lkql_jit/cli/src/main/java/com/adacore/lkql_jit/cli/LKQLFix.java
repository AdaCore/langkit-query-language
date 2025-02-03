//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.cli;

import com.adacore.lkql_jit.options.LKQLOptions;
import java.util.Arrays;
import java.util.Iterator;
import picocli.CommandLine;

/**
 * This class represents the LKQL fixer entry point. The subcommand "fix" of the LKQL driver is
 * mainly used to test auto-fixing functions defined on GNATcheck rules.
 */
public class LKQLFix extends BaseLKQLChecker {

    // ---- Subcommand spec -----

    /** This class defines the "fix" LKQL subcommand. */
    @CommandLine.Command(
        name = "fix",
        description = "Fixer driver, to apply LKQL defined auto-fixes to sources. In this mode, you" +
        " can only enable rules that defines an auto-fixing function."
    )
    public static class Args extends BaseLKQLChecker.Args {

        @CommandLine.Option(
            names = { "--auto-fix-mode" },
            description = "Mode to apply auto fixes (default is DISPLAY)" +
            "%nPossible values: ${COMPLETION-CANDIDATES}",
            completionCandidates = AutoFixModeCompletion.class
        )
        public LKQLOptions.AutoFixMode autoFixMode = LKQLOptions.AutoFixMode.DISPLAY;

        @Override
        public Integer call() {
            new LKQLFix(this).launch(this.unmatched.toArray(new String[0]));
            return 0;
        }
    }

    /** Utility class to provide auto-complete for the auto-fix mode. */
    public static class AutoFixModeCompletion implements Iterable<String> {

        @Override
        public Iterator<String> iterator() {
            return Arrays.stream(LKQLOptions.AutoFixMode.values()).map(Object::toString).iterator();
        }
    }

    // ----- Constructors -----

    public LKQLFix(LKQLFix.Args args) {
        super(args);
    }

    // ----- Instance methods -----

    @Override
    protected LKQLOptions getOptions() {
        return getBaseOptionsBuilder()
            .engineMode(LKQLOptions.EngineMode.FIXER)
            .checkerDebug(true)
            .autoFixMode(((Args) args).autoFixMode)
            .build();
    }
}
