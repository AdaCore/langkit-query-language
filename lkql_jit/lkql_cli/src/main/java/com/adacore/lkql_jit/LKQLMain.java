/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit;

import java.util.concurrent.Callable;
import picocli.CommandLine;
import picocli.CommandLine.Command;

@Command(
        mixinStandardHelpOptions = true,
        name = "lkql",
        synopsisSubcommandLabel = "COMMAND",
        subcommands = {
            LKQLLauncher.LKQLRun.class,
            LKQLChecker.Args.class,
            GNATCheckWorker.Args.class,
            LKQLDoc.class
        },
        description =
                "Unified driver for LKQL (Langkit query language). Allows you to run LKQL "
                        + "scripts or apply specific checks on a given Ada codebase")
public class LKQLMain implements Callable<Integer> {

    @CommandLine.Spec CommandLine.Model.CommandSpec spec;

    @Override
    public Integer call() throws Exception {
        throw new CommandLine.ParameterException(spec.commandLine(), "Missing required subcommand");
    }

    public static void main(String[] args) {
        int rc = new CommandLine(new LKQLMain()).execute(args);
        System.exit(rc);
    }
}
