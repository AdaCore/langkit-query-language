#! /usr/bin/env python

import argparse
import itertools

import liblkqllang as lkql

from prompt_toolkit import PromptSession
from prompt_toolkit.history import FileHistory
from prompt_toolkit.lexers import PygmentsLexer
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


class LKQLCompleter(Completer):
    """
    LKQL completer, based on p_interp_complete.
    """
    def get_completions(self, document, complete_event):

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
    session = PromptSession(
        history=our_history, lexer=PygmentsLexer(lkql.LKQLPygmentsLexer),
        completer=FuzzyCompleter(LKQLCompleter())
    )

    dummy_unit = ctx.get_from_buffer('<dummy>', '12')
    dummy_unit.root.p_interp_init_from_project(args.project)

    for i in itertools.count(start=1):

        # We have a new buffer name at each iteration, to create a new buffer
        # for each repl input, so that references to old buffers are still valid.
        # Due to the way LKQL is interpreted, references to old code can still
        # be made, so we must make sure we don't deallocate old buffers.
        buffer_name = f'<repl_input_{i}>'

        try:
            with patch_stdout():
                cmd = session.prompt('> ')
            if cmd == 'exit':
                break

            cmd_unit = ctx.get_from_buffer(buffer_name, cmd)

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
