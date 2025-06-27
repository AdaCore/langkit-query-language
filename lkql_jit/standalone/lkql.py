#! /usr/bin/env python
"""
Python wrapper to call the JVM version of LKQL JIT.
"""

import argparse
import os
import os.path as P
import subprocess

if __name__ == '__main__':
    # Create an argument parser to catch debug flags and leave remaining
    # options.
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument(
        "--truffle-debug", action="store_true",
        help="Activate many options for truffle that allows the user to debug"
        "the JIT"
    )
    parser.add_argument(
        "--compile-immediately", action="store_true",
        help="Activate an option for truffle that triggers automatic"
        "compilation of root nodes. Useful to debug performance issues"
    )
    args, remaining = parser.parse_known_args()

    # Get the required paths
    graal_home = os.environ["GRAAL_HOME"]
    lkql_jit_home = os.environ.get(
        "LKQL_JIT_HOME", P.join(os.path.dirname(__file__))
    )

    # Ensure the 'lkql_jit_home' is a directory
    if not os.path.isdir(lkql_jit_home):
        print(f"LKQL_JIT_HOME is not a directory: {lkql_jit_home}")
        exit(1)

    # Get the library path and ensure it is a directory
    lib_dir = os.path.join(lkql_jit_home, "lib")
    if not os.path.isdir(lib_dir):
        print("Cannot find  the 'lib' directory in the LKQL_JIT_HOME")
        exit(1)

    # Get the Java executable
    java = P.join(graal_home, "bin", "java.exe" if os.name == "nt" else "java")

    # Create the class path by listing all JARs in 'lkql_jit_home'
    class_path = os.pathsep.join(
        [
            os.path.join(lib_dir, p)
            for p in os.listdir(lib_dir)
            if p.endswith(".jar")
        ]
    )

    # Create the java.library.path property
    java_library_path = os.environ.get(
        "PATH" if os.name == "nt" else "LD_LIBRARY_PATH", ""
    )

    # Run the full Java command
    subprocess.run(
        [
            java,
            "-cp",
            class_path,
            f"-Djava.library.path={java_library_path}",
            "--enable-native-access=ALL-UNNAMED",
            "--sun-misc-unsafe-memory-access=allow",
        ]
        + (
            ["-Dgraal.Dump=Truffle:1", "-Dgraal.PrintGraph=Network"]
            if args.truffle_debug
            else []
        )
        + ["com.adacore.lkql_jit.cli.LKQLMain"]
        + remaining
        + (
            [
                "--experimental-options",
                "--engine.Compilation=true",
                "--engine.TraceCompilation=false",
                "--engine.TraceCompilationDetails=true",
                "--engine.TracePerformanceWarnings=all",
                "--engine.CompilationFailureAction=Diagnose",
                '--vm.XX:+TraceDeoptimization',
                '--engine.NodeSourcePositions',
                '--engine.TraceTransferToInterpreter',
                '--vm.XX:+TraceDeoptimizationDetails',
                '--engine.NodeSourcePositions',
                '--vm.XX:+UnlockDiagnosticVMOptions',
                '--vm.XX:+DebugNonSafepoints',
                '--vm.XX:+TraceDeoptimization',
                '--engine.CompilationStatistics',
                '--engine.BackgroundCompilation=false',
                '--engine.TraceDeoptimizeFrame'
            ]
            if args.truffle_debug
            else []
        )
        + (
            ['--engine.CompileImmediately=true']
            if args.compile_immediately
            else []
        )
    )
