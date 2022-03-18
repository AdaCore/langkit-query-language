#! /usr/bin/env python

from langkit.libmanage import ManageScript


class Manage(ManageScript):

    ENABLE_BUILD_WARNINGS_DEFAULT = True

    EMIT_JAVA_API = True

    @property
    def main_programs(self):
        return super().main_programs | {'lkql_ada'}

    def create_context(self, args):

        from langkit.compile_context import AdaSourceKind, CompileCtx

        from language.lexer import lkql_lexer
        from language.parser import lkql_grammar

        ctx = CompileCtx(lang_name='Lkql',
                         short_name='lkql',
                         lexer=lkql_lexer,
                         grammar=lkql_grammar)

        ctx.add_with_clause('Implementation', AdaSourceKind.body,
                            'Liblkqllang.Prelude', use_clause=True)

        return ctx


if __name__ == '__main__':
    Manage().run()
