//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.cli;

import com.adacore.langkit_support.NativeTools;
import java.util.concurrent.Callable;
import picocli.CommandLine;
import picocli.CommandLine.Command;

@Command(
    mixinStandardHelpOptions = true,
    name = "lkql",
    synopsisSubcommandLabel = "COMMAND",
    subcommands = {
        LKQLLauncher.LKQLRun.class,
        LKQLPasses.LKQLRun.class,
        LKQLChecker.Args.class,
        LKQLFix.Args.class,
        GNATCheckWorker.Args.class,
        LKQLDocAPI.class,
        LKQLDocRules.class,
        LKQLRefactor.class,
    },
    description = "Unified driver for LKQL (Langkit query language). Allows you to run LKQL " +
    "scripts or apply specific checks on a given Ada codebase",
    // Version information are substituted by anod for production builds
    version = { "lkql 26.0w (unknown date)", "Copyright (C) 2004-2025, AdaCore." }
)
public class LKQLMain implements Callable<Integer> {

    @CommandLine.Spec
    CommandLine.Model.CommandSpec spec;

    @Override
    public Integer call() throws Exception {
        throw new CommandLine.ParameterException(spec.commandLine(), "Missing required subcommand");
    }

    public static void main(String[] args) {
        // Start by unsetting the "LD_PRELOAD" environment variable to make
        // sure child processes aren't impacted by it.
        try {
            NativeTools.setenv("LD_PRELOAD", "");
        } catch (Throwable t) {
            System.err.println("Error while calling the 'setenv' function");
            t.printStackTrace();
            System.exit(1);
        }

        // Then call the main processing function
        int rc = new CommandLine(new LKQLMain()).execute(args);
        System.exit(rc);
    }
}
