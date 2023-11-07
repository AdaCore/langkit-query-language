"""
Python wrapper to call the Java version of LKQL JIT.
"""

import argparse
import os
import os.path as P
import subprocess

# Jar for each LKQL JIT entry point
jars = {
    "lkql": "lkql_cli.jar"
}

# Main class for each LKQL JIT entry point
main_classes = {
    "lkql": "LKQLMain"
}

def get_java_command(entry_point: str) -> list[str]:
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
        ("lkql", "Main entry point for LKQL")
    ]:
        subp = subparsers.add_parser(subcommand, help=help)
        subp.set_defaults(subc=subcommand)

    args, to_forward = parser.parse_known_args()
    command = get_java_command(args.subc)
    command.extend(to_forward)
    subprocess.run(command)
