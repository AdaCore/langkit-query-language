#! /usr/bin/env python

import os.path

from langkit.compile_context import CompileCtx
import langkit.config as C
from langkit.libmanage import ManageScript
import langkit.names as names
from langkit.utils import PluginLoader


class Manage(ManageScript):

    ENABLE_BUILD_WARNINGS_DEFAULT = True

    def create_config(self, args):
        return C.CompilationConfig(
            lkt_spec=None,
            library=C.LibraryConfig(
                root_directory=os.path.dirname(__file__),
                language_name=names.Name("Lkql"),
                short_name="lkql",
            ),
        )

    def create_context(self, config, verbosity):
        from language.lexer import lkql_lexer
        from language.parser import lkql_grammar

        return CompileCtx(
            config=config,
            plugin_loader=PluginLoader(config.library.root_directory),
            lexer=lkql_lexer,
            grammar=lkql_grammar,
            verbosity=verbosity,
        )


if __name__ == '__main__':
    Manage().run()
