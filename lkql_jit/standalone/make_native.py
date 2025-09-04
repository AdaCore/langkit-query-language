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
from utils import GraalManager, parse_args, is_windows


def look_for_files_in_env(files: list[str], env_var_name: str) -> dict[str, str] | None:
    """
    Look for required `files` in directories listed in the value of the
    environment variable `env_var_name`. Return the dictionary associating each
    input file to the directory containing it. If one of the requested file
    cannot be retrieved this function returns `None` and displays error message
    about file not being found.
    """
    res = {f: None for f in files}
    for dir in os.environ.get(env_var_name, "").split(os.pathsep):
        for file in files:
            if os.path.isfile(os.path.join(dir, file)):
                res[file] = dir
                break
    one_not_found = False
    for file, dir in res.items():
        if dir is None:
            one_not_found = True
            print(f'Cannot find "{file}" in {env_var_name}')
    return None if one_not_found else res


if __name__ == "__main__":
    # Create utils
    graal = GraalManager()
    args = parse_args()

    # Prepare system dependant native-image options
    os_specific_options = []
    if not is_windows():
        # On Linux, we need to provide the compiler additional arguments for
        # it to find headers and shared objects.
        headers_paths = look_for_files_in_env(
            ["libadalang.h", "liblkqllang.h"],
            "C_INCLUDE_PATH",
        )
        libs_paths = look_for_files_in_env(
            ["libadalang.so", "liblkqllang.so", "libz.so"],
            "LIBRARY_PATH",
        )
        if headers_paths is None or libs_paths is None:
            print("Missing lib dependencies, cannot continue")
            exit(1)

        # We also need to provide rpath-links to the compiler to allow it to
        # find libraries during linking phase.
        ld_library_path = os.environ.get("LD_LIBRARY_PATH")
        rpaths = (
            [f"-Wl,-rpath-link={p}" for p in ld_library_path.split(os.pathsep)]
            if ld_library_path
            else []
        )

        os_specific_options.extend(
            [
                # The G1 garbage collector is only supported on Linux for now
                "--gc=G1",
                # Then we add additional options for the C compiler
                *[
                    f"--native-compiler-options=-I{dir}"
                    for dir in headers_paths.values()
                ],
                *[f"--native-compiler-options=-L{dir}" for dir in libs_paths.values()],
                *[f"--native-compiler-options={rp}" for rp in rpaths],
            ]
        )

    # Run the Native-Image compiler if required by the build process
    if "lkql_cli" in args.native_components:
        # Create the base Native-Image command
        cmd = [
            graal.native_image,
            "-cp",
            args.classpath,
            "--no-fallback",
            "--initialize-at-build-time",
            *os_specific_options,
        ]

        # Add debug and development options
        if args.build_mode in ("dev", "debug"):
            cmd.extend(
                [
                    "-g",
                    "-Ob",
                    "-H:+UnlockExperimentalVMOptions",
                    "-H:-DeleteLocalSymbols",
                    "-H:+SourceLevelDebug",
                    "-H:+PreserveFramePointer",
                    "-H:+IncludeNodeSourcePositions",
                ]
            )
            if args.build_mode == "debug":
                cmd.extend(
                    [
                        "-H:+PrintRuntimeCompileMethods",
                        "-H:+TraceNativeToolUsage",
                    ]
                )

        # Finally specify the main class name and the output file
        final_cmd = cmd + [
            "com.adacore.lkql_jit.cli.LKQLMain",
            P.join("target", "lkql"),
        ]

        # Debug print and run
        print(f"Execute: {final_cmd}", flush=True)
        subprocess.check_call(final_cmd)
