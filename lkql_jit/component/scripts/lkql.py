#! /usr/bin/env python
"""
Python wrapper to call the Java version of LKQL JIT.
"""

import os
import os.path as P
import subprocess
import sys

if __name__ == '__main__':

    # Get the utils paths
    graal_home = os.environ['GRAAL_HOME']
    lkql_jit_home = os.environ.get(
        'LKQL_JIT_HOME',
        P.join(graal_home, 'languages', 'lkql')
    )

    # Get the Java executable
    java = P.join(graal_home, 'bin', 'java.exe' if os.name == 'nt' else 'java')

    # Create the class path
    class_path = os.pathsep.join([
        P.join(graal_home, 'lib', 'truffle', 'truffle-api.jar'),
        P.join(lkql_jit_home, 'lkql_jit.jar'),
        P.join(lkql_jit_home, 'lkql_cli.jar')
    ])

    # Create the java.library.path property
    java_library_path = os.environ.get(
        'PATH' if os.name == 'nt' else "LD_LIBRARY_PATH", ""
    )

    # Run the full command
    subprocess.run([
        java,
        '-cp', class_path,
        f'-Djava.library.path={java_library_path}',
        f'-Dtruffle.class.path.append={P.join(lkql_jit_home, "lkql_jit.jar")}',
        f'com.adacore.lkql_jit.cli.LKQLMain'
    ] + sys.argv[1:])
