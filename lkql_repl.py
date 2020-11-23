#! /usr/bin/env python

import argparse

from collections import defaultdict
import liblkqllang as lkql

from prompt_toolkit import PromptSession
from prompt_toolkit.history import FileHistory
from prompt_toolkit.lexers import PygmentsLexer, Lexer
from prompt_toolkit.styles.named_colors import NAMED_COLORS
from prompt_toolkit.completion import Completer, Completion, FuzzyCompleter
from prompt_toolkit.patch_stdout import patch_stdout

from pygments.lexer import RegexLexer, words
from pygments import token


ctx = lkql.AnalysisContext()


parser = argparse.ArgumentParser()
parser.add_argument(
    "-P", "--project",
    help="Ada project file to base the queries upon",
    required=True
)


class LKQLPygmentsLexer(RegexLexer):
    """
    Pygments lexer for LKQL
    """
    name = 'LKQL'
    filenames = ['*.lkql']

    tokens = {
        'root': [
            (words(('select', 'let', 'when', 'val', 'fun', 'selector',
                    'match', 'rec', 'skip', 'is', 'in', 'true', 'false',
                    'if', 'else', 'then', 'not', 'null'), prefix=r'\b', suffix=r'\b'),
             token.Keyword),
            (r"#(.?)+", token.Comment),
            (r"\b(\-\>|\=\>|\<\=|\>\=|\=|\!\=|and|or|not|\+|\-|\*|\/|\&|"
             r"\@|\||\>|\<)\b", token.Operator),
            (r"\{|\}|\(|\)", token.Punctuation),
            (r"\"[^\"]*\"", token.String),
            (r'[0-9]+', token.Number),
        ]
    }


class LKQLCompleter(Completer):
    """
    LKQL completer, based on p_interp_complete.
    """
    def get_completions(self, document, complete_event):

        word = document.get_word_before_cursor()
        doc = "\n".join(document.lines)

        cmd_unit = ctx.get_from_buffer('<complete>', doc)
        n = cmd_unit.root.lookup(lkql.Sloc(1, len(cmd_unit.text)))

        if n is None:
            return

        for sym in n.p_interp_complete:
            yield Completion(sym)


if __name__ == '__main__':
    args, _ = parser.parse_known_args()
    our_history = FileHistory(".example-history-file")
    session = PromptSession(history=our_history, lexer=PygmentsLexer(LKQLPygmentsLexer),
                            completer=FuzzyCompleter(LKQLCompleter()))

    dummy_unit = ctx.get_from_buffer('<dummy>', '12')
    dummy_unit.root.p_interp_init_from_project(args.project)

    while True:
        try:
            with patch_stdout():
                cmd = session.prompt('> ')
            if cmd == 'exit':
                break

            cmd_unit = ctx.get_from_buffer('<repl_input>', cmd)
            print(cmd_unit.root.p_interp_eval)

        except EOFError:
            try:
                cmd = session.prompt('Do you really want to exit ([y]/n)? ')
                if cmd == 'y' or cmd == '':
                    break
                else:
                    continue
            except EOFError:
                break
