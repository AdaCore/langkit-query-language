"""----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
---------------------------------------------------------------------------"""

# Script to build the LKQL JIT drivers with native-image

"""
Use those options to perform debug on produced LKQL_JIT executables:

-H:+PrintRuntimeCompileMethods  => Use this to debug compilation errors
-H:-DeleteLocalSymbols  => Use this to disable symbol deletion to profile
-H:+SourceLevelDebug  => Use this to preserve link from source to bin
-H:+PreserveFramePointer  => Use this to preserve the frame pointer to profile
-H:+IncludeNodeSourcePositions  => # Use this to debug deoptimizations
-H:+ReportExceptionStackTraces  => ???

We don't use reflection configuration file because all Java classes and
methods are statically required by Libadalang's Java bindings.
According to native-image documentation
(https://www.graalvm.org/latest/reference-manual/native-image/dynamic-features/Reflection/)
Java reflection objects that are statically fetched don't need to be
specified at any other place.
"""

import os
import os.path as P
import subprocess
import sys

sys.path.append("..")
from utils import GraalManager, parse_args, missing_module

if __name__ == "__main__":
    # Create the dir hierarchy
    os.makedirs(P.join(P.dirname(__file__), "bin"), exist_ok=True)

    # Create utils
    graal = GraalManager()
    args = parse_args()

    # Get the language and CLI JARs and verify their existences
    language_jar = P.join("..", "language", "target", "lkql_jit.jar")
    if not P.isfile(language_jar):
        raise missing_module("language", language_jar)

    cli_jar = P.join("..", "cli", "target", "lkql_cli.jar")
    if not P.isfile(cli_jar):
        raise missing_module("cli", cli_jar)

    # Run the Native-Image compiler if required by the build process
    if "lkql_cli" in args.native_components:
        # Create the base Native-Image command
        cmd = [
            graal.native_image,
            "--macro:truffle",
            "--no-fallback",
            "--language:regex",
            "--initialize-at-build-time=com.adacore.lkql_jit,com.adacore.liblkqllang",
        ]

        if args.build_mode in ("dev", "debug"):
            cmd.extend(
                [
                    "-g",
                    "-O0",
                    "-H:-DeleteLocalSymbols",
                    "-H:+SourceLevelDebug",
                    "-H:+PreserveFramePointer",
                    "-H:+IncludeNodeSourcePositions",
                ]
            )
            if args.build_mode == "debug":
                cmd.append("-H:+PrintRuntimeCompileMethods")

        final_cmd = cmd + [
            "-cp",
            os.pathsep.join([language_jar, cli_jar]),
            f"com.adacore.lkql_jit.LKQLMain",
            P.join("bin", "lkql"),
        ]

        # Debug print and run
        print(f"Execute: {final_cmd}", flush=True)
        subprocess.check_call(final_cmd)
