#! /usr/bin/env python

import os

from langkit.libmanage import ManageScript


class Manage(ManageScript):
    def create_context(self, args):
        from langkit.compile_context import CompileCtx

        from language.lexer import lkql_lexer
        from language.parser import lkql_grammar

        return CompileCtx(lang_name='LKQL',
                          lexer=lkql_lexer,
                          grammar=lkql_grammar)

if __name__ == '__main__':
    Manage().run()
