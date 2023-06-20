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
--                                                                          --
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

sys.path.append('..')
from utils import GraalManager, parse_args, missing_module

if __name__ == '__main__':
    # Create the dir hierarchy
    os.makedirs(P.join(P.dirname(__file__), "bin"), exist_ok=True)

    # Create utils
    graal = GraalManager()
    args = parse_args()

    # Get the components to build
    build_launcher = 'launcher' in args.native_components
    build_checker = 'checker' in args.native_components
    build_worker = 'gnatcheck_worker' in args.native_components

    # Create the common command
    cmd = [
        graal.native_image,
        "--macro:truffle",
        "--no-fallback",
        "--language:regex",
        "--initialize-at-build-time=com.adacore.lkql_jit",
    ]

    # Handle the dev and debug build mode
    if args.build_mode in ('dev', 'debug'):
        cmd.extend([
            "-H:-DeleteLocalSymbols",
            "-H:+SourceLevelDebug",
            "-H:+PreserveFramePointer",
            "-H:+IncludeNodeSourcePositions",
        ])
        if args.build_mode == 'debug':
            cmd.append("-H:+PrintRuntimeCompileMethods")

    # Get the language JAR and verify its existence
    language_jar = P.join("..", "language", "target", "lkql_jit.jar")
    if not P.isfile(language_jar):
        raise missing_module("language", language_jar)

    for name, do_native_build, jar_filename, exe_name, class_name in [
        (
                "launcher",
                build_launcher,
                P.join("..", "launcher", "target", "lkql_jit_launcher.jar"),
                "native_lkql_jit",
                "LKQLLauncher"
        ),
        (
                "checker",
                build_checker,
                P.join("..", "checker", "target", "lkql_jit_checker.jar"),
                "native_lkql_jit_checker",
                "LKQLChecker"
        ),
        (
                "gnatcheck_worker",
                build_worker,
                P.join("..", "gnatcheck_worker", "target", "gnatcheck_worker.jar"),
                "native_gnatcheck_worker",
                "GNATCheckWorker"
        ),
    ]:
        if do_native_build:
            if not P.isfile(jar_filename):
                raise missing_module(name, jar_filename)

            class_path = os.pathsep.join([language_jar, jar_filename])
            final_cmd = cmd + [
                "-cp", class_path,
                f"com.adacore.lkql_jit.{class_name}",
                P.join("bin", exe_name)
            ]
            print(f"Execute: {final_cmd}", flush=True)
            subprocess.check_call(final_cmd)
