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
"""

import sys
import os
import os.path as P
import subprocess

sys.path.append('..')
from utils import GraalManager, parse_args

if __name__ == '__main__':
    # Create the dir hierarchy
    os.makedirs(P.join(P.dirname(__file__), "bin"), exist_ok=True)

    # Create utils
    graal = GraalManager()
    args = parse_args()

    # Get the components to build
    build_launcher = 'launcher' in args.native_components
    build_checker = 'checker' in args.native_components

    # Create the common command
    cmd = [
        graal.native_image,
        "--macro:truffle",
        "--no-fallback",
        "--language:regex",
        "--initialize-at-build-time=com.adacore.lkql_jit",
        "-H:ReflectionConfigurationFiles=reflect_config_lal.json,reflect_config_lkql.json",
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

    # Call the building for each native component to build
    if build_launcher:
        launcher_cmd = cmd + [
            "-cp", "../language/target/lkql_jit.jar:../launcher/target/lkql_jit_launcher.jar",
            "com.adacore.lkql_jit.LKQLLauncher",
            "bin/native_lkql_jit"
        ]
        print(f"Execute: {launcher_cmd}", flush=True)
        subprocess.run(launcher_cmd)

    if build_checker:
        checker_cmd = cmd + [
            "-cp", "../language/target/lkql_jit.jar:../checker/target/lkql_jit_checker.jar",
            "com.adacore.lkql_jit.LKQLChecker",
            "bin/native_lkql_jit_checker"
        ]
        print(f"Execute: {checker_cmd}", flush=True)
        subprocess.run(checker_cmd)
