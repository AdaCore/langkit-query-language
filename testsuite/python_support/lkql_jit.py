#! /usr/bin/env python
"""
Python wrapper to call the Java version of LKQL JIT.
"""

import os
import os.path as P
import subprocess
import argparse

if __name__ == "__main__":

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

    # Get the utils paths
    graal_home = os.environ["GRAAL_HOME"]
    lkql_jit_home = os.environ.get(
        "LKQL_JIT_HOME", P.join(graal_home, "languages", "lkql")
    )

    # Get the Java executable
    java = P.join(graal_home, "bin", "java.exe" if os.name == "nt" else "java")

    # Create the class path
    class_path = os.pathsep.join(
        [
            P.join(graal_home, "lib", "truffle", "truffle-api.jar"),
            P.join(lkql_jit_home, "lkql_jit.jar"),
            P.join(lkql_jit_home, "lkql_cli.jar"),
        ]
    )

    # Create the java.library.path property
    java_library_path = os.environ.get(
        "PATH" if os.name == "nt" else "LD_LIBRARY_PATH", ""
    )

    # Run the full command
    subprocess.run(
        [java, "-cp", class_path, f"-Djava.library.path={java_library_path}"]
        + (
            ["-Dgraal.Dump=Truffle:1", "-Dgraal.PrintGraph=Network"]
            if args.truffle_debug
            else []
        )
        + [
            f'-Dtruffle.class.path.append={P.join(lkql_jit_home, "lkql_jit.jar")}',
            "--add-opens=org.graalvm.truffle/com.oracle.truffle.api.strings=ALL-UNNAMED",
            "com.adacore.lkql_jit.cli.LKQLMain",
        ]
        + remaining
        + ([
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
            else [])
        + (['--engine.CompileImmediately=true']
           if args.compile_immediately else [])
    )
