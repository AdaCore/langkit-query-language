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

# Script to make a GraalVM component for the LKQL implementation

"""
To create a GraalVM component, we must create a directory as following :

- my_comp_dir/
|   - languages/
|   |   - my_language/
|   |   |   - bin/   ==> Directory that contains all executable files (startup scripts,
|   |   |   |               native image...)
|   |   |   |   - my_language.sh
|   |   |   |   - my_language
|   |   |
|   |   |   - launcher/   ==> Directory that contains the language launcher if there
|   |   |   |                    is one
|   |   |   |   - my_language_launcher.jar
|   |   |
|   |   |   - my_language.jar   ==> Your language Truffle implementation compiled in a
|   |   |                              JAR file
|   |   |   - native-image.properties   ==> The properties of the language native image
|
|   - META-INF/
|   |   - MANIFEST.MF   ==> The manifest file to declare the language properties and
|   |                          the installation process
|   |   - symlinks   ==> The symbolic links to create to the binary files, there will
|   |                       be placed in the GRAAL_HOME/bin
|   |   - permissions   ==> The binary files permissions to ensure the executability
                               by users

After that we bundle it using the "jar" command and install it with Graal Updater (gu)
binary
"""

import os
import os.path as P
import sys
import shutil
import subprocess

sys.path.append('..')
from utils import GraalManager, parse_args

if __name__ == '__main__':
    # Create the utils
    graal = GraalManager()
    args = parse_args()

    # Create the hierarchy
    comp_dir = P.realpath(P.join(P.dirname(__file__), 'lkql_jit_component'))
    shutil.rmtree(comp_dir, ignore_errors=True)
    meta_dir, lang_dir, bin_dir = graal.component_template(comp_dir)

    # Get the native components to include
    include_native_launcher = "launcher" in args.native_components
    include_native_checker = "checker" in args.native_components

    # Copy the produced JAR to the component
    shutil.copy(
        P.join("..", "language", "target", "lkql_jit.jar"),
        P.join(lang_dir, "lkql_jit.jar")
    )
    shutil.copy(
        P.join("..", "launcher", "target", "lkql_jit_launcher.jar"),
        P.join(lang_dir, "lkql_jit_launcher.jar")
    )
    shutil.copy(
        P.join("..", "checker", "target", "lkql_jit_checker.jar"),
        P.join(lang_dir, "lkql_jit_checker.jar")
    )

    # Copy the GraalVM launching scripts
    shutil.copy(
        P.join("scripts", "lkql_jit"),
        P.join(bin_dir, "lkql_jit")
    )
    shutil.copy(
        P.join("scripts", "lkql_jit_checker"),
        P.join(bin_dir, "lkql_jit_checker")
    )

    # Copy the needed native images
    if include_native_launcher:
        shutil.copy(
            P.join("..", "native", "bin", "lkql_jit"),
            P.join(bin_dir, "native_lkql_jit")
        )
    if include_native_checker:
        shutil.copy(
            P.join("..", "native", "bin", "lkql_jit_checker"),
            P.join(bin_dir, "native_lkql_jit_checker")
        )

    # Create the needed file to compile in jar
    open(P.join(lang_dir, "native-image.properties"), 'w').close()

    # Write the manifest
    with open(P.join(meta_dir, "MANIFEST.MF"), 'w') as f:
        f.writelines([
            "Bundle-Name: Langkit Query Language JIT\n",
            "Bundle-Symbolic-Name: com.adacore.lkql_jit\n",
            f"Bundle-Version: {args.lkql_version}\n",
            "Bundle-RequireCapability: org.graalvm; " +
            f"filter:=\"(&(graalvm_version={args.graal_version})(os_arch=amd64))\"\n",
            "x-GraalVM-Polyglot-Part: True\n"
        ])

    # Write the symbolic links
    # TODO: create symlink to native build or interpreter version depending on a setting
    # chosen at installation time?
    with open(P.join(meta_dir, "symlinks"), 'w') as f:
        f.writelines([
            (
                "bin/lkql_jit = ../languages/lkql/bin/lkql_jit\n"
                if include_native_launcher else
                ""
            ),
            (
                "bin/lkql_jit_checker = ../languages/lkql/bin/lkql_jit_checker\n"
                if include_native_checker else
                ""
            ),
        ])

    # Write the permissions file
    with open(P.join(meta_dir, "permissions"), 'w') as f:
        f.writelines([
            "languages/lkql/bin/lkql_jit = rwxrwxr-x\n",
            "languages/lkql/bin/lkql_jit_checker = rwxrwxr-x\n",
            (
                "languages/lkql/bin/native_lkql_jit = rwxrwxr-x\n"
                if include_native_launcher else
                ""
            ),
            (
                "languages/lkql/bin/native_lkql_jit_checker = rwxrwxr-x\n"
                if include_native_checker else
                ""
            ),
        ])

    # Create the component jar
    os.chdir(comp_dir)
    subprocess.run([
        graal.jar,
        "cfm",
        P.join("..", "lkql_jit_component.jar"),
        P.join("META-INF", "MANIFEST.MF"),
        ".",
    ])

    subprocess.run([
        graal.jar,
        "uf",
        P.join("..", "lkql_jit_component.jar"),
        P.join("META-INF", "symlinks"),
    ])

    subprocess.run([
        graal.jar,
        "uf",
        P.join("..", "lkql_jit_component.jar"),
        P.join("META-INF", "permissions"),
    ])
