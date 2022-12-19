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
----------------------------------------------------------------------------"""

import os
import sys
import shutil
import subprocess

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

# Get the environment variable
graal_home = None
try:
    graal_home = os.environ["GRAAL_HOME"]
except KeyError:
    print(
        "[\033[91mNATIVE-BUILD\033[0m] Please define the GRAAL_HOME environment " +
        "variable to the GraalVM root directory :)"
    )
    exit(1)

jar_exec = os.path.join(graal_home, "bin", "jar")

# Get the arguments
if len(sys.argv) < 3:
    print("Please provide arguments : LKQL_VERSION GRAAL_VERSION")
    exit(1)

lkql_version = sys.argv[1]
graal_version = sys.argv[2]
include_checker = "--checker" in sys.argv

# Set the directory names
comp_dir = "comp_temp"
meta_dir = os.path.join(comp_dir, "META-INF")
lang_dir = os.path.join(comp_dir, "languages", "lkql")
bin_dir = os.path.join(lang_dir, "bin")

# Create the directory and organize the jar to build
shutil.rmtree(comp_dir, ignore_errors=True)

os.makedirs(meta_dir, exist_ok=True)
os.makedirs(lang_dir, exist_ok=True)
os.makedirs(bin_dir, exist_ok=True)

shutil.copy(
    os.path.join("..", "language", "target", "lkql_jit.jar"),
    os.path.join(lang_dir, "lkql_jit.jar")
)

shutil.copy(
    os.path.join("..", "native", "bin", "lkql_jit"),
    os.path.join(bin_dir, "lkql_jit")
)
if include_checker:
    shutil.copy(
        os.path.join("..", "native", "bin", "lkql_jit_checker"),
        os.path.join(bin_dir, "lkql_jit_checker")
    )

# Create the needed file to compile in jar
f = open(os.path.join(lang_dir, "native-image.properties"), 'w')
f.close()

# Write the manifest
f = open(os.path.join(meta_dir, "MANIFEST.MF"), 'w')
f.writelines([
    "Bundle-Name: Langkit Query Language JIT\n",
    "Bundle-Symbolic-Name: com.adacore.lkql_jit\n",
    "Bundle-Version: {}\n".format(lkql_version),
    "Bundle-RequireCapability: org.graalvm; " +
    "filter:=\"(&(graalvm_version={})(os_arch=amd64))\"\n".format(graal_version),
    "x-GraalVM-Polyglot-Part: True\n"
])
f.close()

# Write the symbolic links
f = open(os.path.join(meta_dir, "symlinks"), 'w')
f.writelines([
    "bin/lkql_jit = ../languages/lkql/bin/lkql_jit\n",
    ("bin/lkql_jit_checker = ../languages/lkql/bin/lkql_jit_checker\n"
        if include_checker
        else "")
])
f.close()

# Write the permissions file
f = open(os.path.join(meta_dir, "permissions"), 'w')
f.writelines([
    "languages/lkql/bin/lkql_jit = rwxrwxr-x\n",
    ("languages/lkql/bin/lkql_jit_checker = rwxrwxr-x\n"
        if include_checker
        else "")
])
f.close()

# Create the component jar
os.chdir(comp_dir)
command = (
    jar_exec,
    "cfm",
    os.path.join("..", "lkql_jit_component.jar"),
    os.path.join("META-INF", "MANIFEST.MF"),
    "."
)
subprocess.run(command)

command = (
    jar_exec,
    "uf",
    os.path.join("..", "lkql_jit_component.jar"),
    os.path.join("META-INF", "symlinks")
)
subprocess.run(command)

command = (
    jar_exec,
    "uf",
    os.path.join("..", "lkql_jit_component.jar"),
    os.path.join("META-INF", "permissions")
)
subprocess.run(command)
