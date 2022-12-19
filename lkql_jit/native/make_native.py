"""----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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

import os
import sys
import subprocess

graal_home = None
try:
    graal_home = os.environ["GRAAL_HOME"]
except KeyError:
    print("[\033[91mNATIVE-BUILD\033[0m] Please define the GRAAL_HOME environment" +
          " variable to the GraalVM root directory :)")
    exit(1)

native_exec = os.path.join(graal_home, "bin", "native-image")

if not os.path.isfile(os.path.realpath(native_exec)):
    print("[\033[91mNATIVE-BUILD\033[0m] Install the native-image module on your" +
          " GraalVM installation :)")
    exit(1)

os.makedirs("bin", exist_ok=True)

build_checker = "--checker" in sys.argv

"""
Use those option to perform debug on the compilation process
-H:+PrintRuntimeCompileMethods  => Use this to debug compilation errors
-H:-DeleteLocalSymbols  => Use this to disable symbol deletion to profile
-H:+SourceLevelDebug  => Use this to preserve link from source to bin
-H:+PreserveFramePointer  => Use this to preserve the frame pointer to profile
-H:+IncludeNodeSourcePositions  => # Use this to debug deoptimizations
-H:+ReportExceptionStackTraces  => ???
"""

common_command = (
    native_exec,
    "--macro:truffle",
    "--no-fallback",
    "-H:-DeleteLocalSymbols",
    "-H:+SourceLevelDebug",
    "-H:+PreserveFramePointer",
    "--language:regex",
    "--initialize-at-build-time=com.adacore.lkql_jit",
    "-H:ReflectionConfigurationFiles=reflect_config_lada.json,reflect_config_lkql.json",
)

# Create the LKQL launcher
command = common_command + (
    "-cp", "../language/target/lkql_jit.jar:../launcher/target/lkql_jit_launcher.jar",
    "com.adacore.lkql_jit.LKQLLauncher",
    "bin/lkql_jit"
)
subprocess.run(command)

# Create the LKQL checker if needed
if build_checker:
    command = common_command + (
        "-cp", "../language/target/lkql_jit.jar:../checker/target/lkql_jit_checker.jar",
        "com.adacore.lkql_jit.LKQLChecker",
        "bin/lkql_jit_checker"
    )
    subprocess.run(command)

# TODO : Create a shared lib with native image
