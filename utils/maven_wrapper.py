"""
This script is a wrapper around the Maven command, it is used by the pre-commit
hooks configuration to simplify the process of running Maven on LKQL JIT
sources.
"""

import os
import subprocess
import sys

if __name__ == '__main__':
    # Get the Maven executable
    mvn = "mvn.cmd" if os.name == "nt" else "mvn"

    # Get the path of the LKQL JIT "pom.xml" file
    pom_xml = os.path.abspath(
        os.path.join(
            os.path.dirname(os.path.realpath(__file__)),
            "..",
            "lkql_jit",
            "pom.xml",
        )
    )

    # Forward script arguments to Maven and ensure 'MAVEN_ARGS' is taken into
    # account.
    maven_args = sys.argv[1:] + (
        os.environ['MAVEN_ARGS'].split(" ")
        if os.environ.get('MAVEN_ARGS') else
        []
    )

    # Set maven to offline mode when running in a Gitlab CI
    if "CI_JOB_NAME" in os.environ:
        maven_args = [
            "-o",
            *maven_args,
        ]

    # Run Maven on LKQL JIT project with additional args of this script
    try:
        subprocess.check_output([
            mvn,
            "-f",
            pom_xml,
            "-q",
            *maven_args
        ])
    except subprocess.CalledProcessError as e:
        print(e.output.decode())
        exit(e.returncode)
