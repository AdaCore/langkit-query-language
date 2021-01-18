#! /usr/bin/env python

import argparse
import itertools

import liblkqllang as lkql

from prompt_toolkit import PromptSession, print_formatted_text
from prompt_toolkit.history import FileHistory
from prompt_toolkit.lexers import PygmentsLexer
from prompt_toolkit.completion import Completer, Completion, FuzzyCompleter
from prompt_toolkit.patch_stdout import patch_stdout
from prompt_toolkit.styles import Style
from prompt_toolkit.formatted_text import HTML


style = Style.from_dict({
    'prompt': 'ansigreen bold',
    'logo':   'yellow bold'
})


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


builtins = {}
global dummy_unit

HELP_MESSAGE = """
LKQL read eval print loop. You can use it to evaluate LKQL statements on an Ada
project
""".strip()

WELCOME_PROMPT = r"""
<logo>.-.   .-. .-..----. .-.    </logo>
<logo>| |   | |/ //  {}  \| |    </logo>    Welcome to LKQL repl
<logo>| `--.| |\ \\      /| `--. </logo>    type 'help' for more information
<logo>`----'`-' `-'`-----``----' </logo>
"""


def register_builtin(fn):
    builtins[fn.__name__] = fn
    return fn


@register_builtin
def exit():
    """
    Exit the REPL
    """
    raise EOFError()


@register_builtin
def reload():
    """
    Reload the Ada sources the REPL was started on
    """
    print("Reloading sources ...")
    dummy_unit.root.p_interp_init_from_project(args.project)


@register_builtin
def help():
    """
    Print the help
    """

    commands_help = "\n".join(
        f"{fn.__name__}: {fn.__doc__}" for fn in builtins.values()
    )
    print(f"{HELP_MESSAGE}\n\n{commands_help}")


if __name__ == '__main__':
    print_formatted_text(HTML(WELCOME_PROMPT), style=style)

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
        # for each repl input, so that references to old buffers are still
        # valid.
        # Due to the way LKQL is interpreted, references to old code can still
        # be made, so we must make sure we don't deallocate old buffers.
        buffer_name = f'<repl_input_{i}>'

        try:
            with patch_stdout():
                cmd = session.prompt(HTML("<prompt> > </prompt>"), style=style)

            if cmd.strip() in builtins.keys():
                builtins[cmd.strip()]()
            else:
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
