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

# Script to make a GraalVM component for the LKQL implementation

"""
To create a GraalVM component, we must create a directory as following :

- my_comp_dir/
|   - languages/
|   |   - my_language/
|   |   |   - bin/   ==> Directory that contains all executable files (startup scripts,
|   |   |   |            native image...)
|   |   |   |   - my_language.sh
|   |   |   |   - my_language
|   |   |
|   |   |   - launcher/   ==> Directory that contains the language launcher if there
|   |   |   |                 is one
|   |   |   |   - my_language_launcher.jar
|   |   |
|   |   |   - my_language.jar   ==> Your language Truffle implementation compiled in a
|   |   |                           JAR file
|   |   |   - native-image.properties   ==> The properties of the language native image
|
|   - META-INF/
|   |   - MANIFEST.MF   ==> The manifest file to declare the language properties and
|   |                       the installation process
|   |   - symlinks      ==> The symbolic links to create to the binary files, there will
|   |                       be placed in the GRAAL_HOME/bin
|   |   - permissions   ==> The binary files permissions to ensure the executability
                            by users

After that we bundle it using the "jar" command and install it with Graal Updater (gu)
binary
"""

import os
import os.path as P
import shutil
import subprocess
import sys

sys.path.append("..")
from utils import (
    GraalManager,
    parse_args,
    is_windows,
    component_template,
    missing_module,
)

if __name__ == "__main__":
    graal = GraalManager()
    args = parse_args()

    # Create the component hierarchy
    comp_dir = P.realpath(P.join(P.dirname(__file__), "lkql_jit_component"))
    shutil.rmtree(comp_dir, ignore_errors=True)
    meta_dir, lang_dir, bin_dir = component_template(comp_dir)

    # Get if the LKQL native executable should be included in the component
    include_native_lkql = "lkql_cli" in args.native_components

    # Copy the produced JARs into the component
    for name, source_filename in [
        ("language", P.join("..", "language", "target", "lkql_jit.jar")),
        ("cli", P.join("..", "cli", "target", "lkql_cli.jar")),
    ]:
        # Ensure the JAR has been produced
        if not P.isfile(source_filename):
            raise missing_module(name, source_filename)

        shutil.copy(source_filename, P.join(lang_dir))

    # Copy the LKQL Python script into the component
    shutil.copy(P.join("scripts", "lkql.py"), P.join(bin_dir, "lkql.py"))

    # Copy the LKQL native executable if required by the build process
    if include_native_lkql:
        shutil.copy(
            P.join(
                "..",
                "native",
                "bin",
                "lkql.exe" if is_windows() else "lkql"
            ),
            P.join(bin_dir, "lkql"),
        )

    # Create the native image properties file
    open(P.join(lang_dir, "native-image.properties"), "w").close()

    # Write the manifest
    with open(P.join(meta_dir, "MANIFEST.MF"), "w") as f:
        f.writelines(
            [
                "Bundle-Name: Langkit Query Language JIT\n",
                "Bundle-Symbolic-Name: com.adacore.lkql_jit\n",
                f"Bundle-Version: {args.lkql_version}\n",
                "Bundle-RequireCapability: org.graalvm; "
                + f'filter:="(&(graalvm_version={args.graal_version})(os_arch=amd64))"\n',
                "x-GraalVM-Polyglot-Part: True\n",
            ]
        )

    # Write the symbolic links
    with open(P.join(meta_dir, "symlinks"), "w") as f:
        if include_native_lkql:
            f.writelines(["bin/lkql = ../languages/lkql/bin/lkql\n"])
        elif not is_windows():
            f.writelines(["bin/lkql = ../languages/lkql/bin/lkql.py\n"])

    # Write the permissions file
    with open(P.join(meta_dir, "permissions"), "w") as f:
        f.writelines([
            "languages/lkql/bin/lkql = rwxrwxr-x\n",
            "languages/lkql/bin/lkql.py = rwxrwxr-x\n",
        ])

    # Create the component jar
    os.chdir(comp_dir)
    subprocess.check_call(
        [
            graal.jar,
            "cfm",
            P.join("..", "lkql_jit_component.jar"),
            P.join("META-INF", "MANIFEST.MF"),
            ".",
        ]
    )

    if not is_windows():
        subprocess.check_call(
            [
                graal.jar,
                "uf",
                P.join("..", "lkql_jit_component.jar"),
                P.join("META-INF", "symlinks"),
            ]
        )

    subprocess.check_call(
        [
            graal.jar,
            "uf",
            P.join("..", "lkql_jit_component.jar"),
            P.join("META-INF", "permissions"),
        ]
    )
