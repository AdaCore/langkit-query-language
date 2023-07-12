"""
Python wrapper to call the Java version of LKQL JIT.
"""

import argparse
import os
import os.path as P
import subprocess

# Jar for each LKQL JIT entry point
jars = {
    "launcher": "lkql_jit_launcher.jar",
    "checker": "lkql_jit_checker.jar",
    "gnatcheck_worker": "gnatcheck_worker.jar"
}

# Main class for each LKQL JIT entry point
main_classes = {
    "launcher": "LKQLLauncher",
    "checker": "LKQLChecker",
    "gnatcheck_worker": "GNATCheckWorker"
}

<<<<<<< Updated upstream
def show_usage():
    """
    Show the usage of the script.
    """
    print("lkql_jit.py usage: lkql_jit.py [mode] [args...]")
    print("Available modes are; launcher, checker, gnatcheck_worker")

def get_java_command(mode):
=======
def get_java_command(entry_point: str) -> list[str]:
>>>>>>> Stashed changes
    """
    Get the Java command as a list of string to run the given LKQL JIT entry point.
    """
    # Get the utis paths
    graal_home = os.environ['GRAAL_HOME']
    lkql_jit_home = os.environ.get(
        'LKQL_JIT_HOME',
        P.join(graal_home, 'languages', 'lkql')
    )

    # Get the Java executable
    java = (
        P.join(graal_home, 'bin', 'java.exe')
        if os.name == 'nt' else
        P.join(graal_home, 'bin', 'java')
    )

    # Create the class path
    class_path = os.pathsep.join([
        P.join(graal_home, 'lib', 'truffle', 'truffle-api.jar'),
        P.join(lkql_jit_home, 'lkql_jit.jar'),
        P.join(lkql_jit_home, jars[entry_point])
    ])

    # Create the java.library.path property
    java_library_path = (
        os.environ.get('PATH', "")
        if os.name == 'nt' else
        os.environ.get('LD_LIBRARY_PATH', "")
    )

    # Return the full command
    return [
        java,
        '-cp', class_path,
        f'-Djava.library.path={java_library_path}',
        f'-Dtruffle.class.path.append={P.join(lkql_jit_home, "lkql_jit.jar")}',
        f'com.adacore.lkql_jit.{main_classes[entry_point]}'
    ]

if __name__ == '__main__':
    # Create the script argument parser
    parser = argparse.ArgumentParser(prog="lkql_jit.py",
                                     description=__doc__)
    subparsers = parser.add_subparsers(help="LKQL JIT entry point", required=True)
    for subcommand, help in [
        ("launcher", "Use LKQL JIT in normal mode."),
        ("checker", "Use the LKQL JIT checker driver."),
        ("gnatcheck_worker", "Use the LKQL JIT GNATcheck worker (this should be only used by GNATcheck).")
    ]:
        subp = subparsers.add_parser(subcommand, help=help)
        subp.set_defaults(subc=subcommand)

    args, to_forward = parser.parse_known_args()
    command = get_java_command(args.subc)
    command.extend(to_forward)
    subprocess.run(command)
