#! /usr/bin/env python

import os

from langkit.libmanage import ManageScript


class Manage(ManageScript):

    @property
    def main_programs(self):
        return super().main_programs | {'lkql_ada'}

    def create_context(self, args):

        from langkit.compile_context import (CompileCtx, ADA_BODY)

        from language.lexer import lkql_lexer
        from language.parser import lkql_grammar

        ctx = CompileCtx(lang_name='LKQL',
                         lexer=lkql_lexer,
                         grammar=lkql_grammar)

        ctx.add_with_clause('Implementation', ADA_BODY, 'Liblkqllang.Prelude',
                            use_clause=True)

        return ctx


if __name__ == '__main__':
    Manage().run()
