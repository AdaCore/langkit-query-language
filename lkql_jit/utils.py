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

# This python file contains utils for the building process of LKQL JIT

import argparse
import os
import os.path as P


class GraalManager:
    """
    Class which helps other building script to access the local GraalVM
    installation and binaries.
    """

    _native_image = None

    def __init__(self):
        """
        Initialize the Graal manager.
        """
        self.home = P.realpath(os.environ.get('GRAAL_HOME'))

        # Verify the GraalVM installation
        if not self.home:
            raise RuntimeError("Define the 'GRAAL_HOME' environment variable on your "
                               "local GraalVM installation directory")

    # --- Binaries getting ---

    @property
    def native_image(self):
        if not self._native_image:
            self._native_image = (
                P.join(self.home, 'bin', 'native-image.cmd')
                if is_windows() else
                P.join(self.home, 'bin', 'native-image')
            )
            if not P.isfile(self._native_image):
                raise RuntimeError("Native-image tool is not available in your GraalVM"
                                   "installation, install it with 'gu'")
        return self._native_image

    @property
    def jar(self):
        return (
            P.join(self.home, 'bin', 'jar.exe')
            if is_windows() else
            P.join(self.home, 'bin', 'jar')
        )

    @property
    def gu(self):
        return (
            P.join(self.home, 'bin', 'gu.cmd')
            if is_windows() else
            P.join(self.home, 'bin', 'gu')
        )


def component_template(dir):
    """
    Create a graal component directory hierarchy in the given directory.

    :param dir: The directory to create the hierarchy.
    :return The directories from the created hierarchy.
    """
    meta_dir = P.join(dir, 'META-INF')
    lang_dir = P.join(dir, 'languages', 'lkql')
    bin_dir = P.join(lang_dir, 'bin')

    os.makedirs(dir)
    os.makedirs(meta_dir)
    os.makedirs(lang_dir)
    os.makedirs(bin_dir)

    return meta_dir, lang_dir, bin_dir


def is_windows():
    """
    Return if the current platform is Windows.
    """
    return os.name == 'nt'


def parse_args():
    """
    Get the building arguments stored in an argparse object.

    :return: The arguments in an object.
    """
    # Create the parser
    parser = argparse.ArgumentParser()

    # Add the options
    parser.add_argument('--build-mode',
                        choices=['dev', 'prod', 'debug'],
                        default='dev',
                        help="Define the JIT build mode")
    parser.add_argument('--native-components',
                        help="Native components to build")
    parser.add_argument('--lkql-version',
                        help="The version of LKQL you're building")
    parser.add_argument('--graal-version',
                        help="Version of GraalVM you're building with")

    # Parse the command line and return the result
    return parser.parse_args()


def missing_module(name, file):
    return RuntimeError(f"File '{P.realpath(file)}' not found. Make sure you ran `mvn package`"
                        f" for the '{name}' module.")
