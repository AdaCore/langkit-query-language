import liblkqllang
import sys

from dataclasses import dataclass
from enum import Enum
from collections import defaultdict
import subprocess

"""
Small refactoring framework for LKQL scripts, based upon liblkqllang.

The principle is very basic, but allows relatively easy automated edits on LKQL
file by attaching actions to tokens.

Actions can be either "append", "prepend", or "replace".

This has the advantage of:

* Contrary to tree rewritings, it can emit a "not yet correct" syntax, which is
  useful for syntax changes

* Contrary to editing the text directly, you don't have to handle the complex
  work of keeping your line/column buffer correct as you apply edits. You can
  apply every action attached to each tokens in sequence

To see examples of refactorings, see the other files in this script's folder
"""

class ActionKind(Enum):
    """
    Kind for an action attached to a token.
    """
    append = 1
    prepend = 2
    replace = 3


@dataclass
class Action:
    """
    Refactoring action. Can be of either kind:

    * "append": Insert the attached text after the token
    * "prepend": Insert the attached text before the token
    * "replace": Completely replace the token by the attached text
    """
    kind: ActionKind
    text: str

def first_with_pred(tok, pred):
    """
    Helped function. Given a starting token, return the first token in the
    chain that satisfies function ``pred``.
    """
    while not(pred(tok)):
        tok = tok.next
    return tok

class Refactor(liblkqllang.App):
    """
    Main class for the refactor app. Refactorings just have to subclass this
    and provide their own ``preprocess`` method.

    By default, running this base app will do nothing.
    """

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.actions = defaultdict(list)

    def add_arguments(self):
        self.parser.add_argument(
            '-I', '--inplace', action='store_true',
            default=False,
            help='Apply changes in-place'
        )

        self.parser.add_argument(
            '--new-lkql-parser', type=str, default='',
            help='Path to the new lkql parser executable. if provided, '
            'try to run it on the refactored source'
        )

    def process_unit(self, unit):
        if unit.diagnostics:
            print("Not processing ", unit.filename)
            return

        self.preprocess(unit)
        self.print_all_tokens(unit)
        if self.args.inplace and self.args.new_lkql_parser != '':
            out = subprocess.check_output([self.args.new_lkql_parser, '-s', '-f', unit.filename], encoding='utf-8')
            if out:
                print("Errors with the new parser in file ", unit.filename)
                print(out)

    def print_all_tokens(self, unit):
        def write_all(f):
            for t in unit.iter_tokens():
                actions = self.actions[t]
                if actions:
                    rep = [act for act in actions if act.kind == ActionKind.replace]
                    assert len(rep) <= 1, "There can be only one replace action"

                    prep = [act for act in actions if act.kind == ActionKind.prepend]
                    app = [act for act in actions if act.kind == ActionKind.append]

                    for act in prep:
                        f.write(act.text)

                    if rep:
                        f.write(rep[0].text)
                    else:
                        f.write(t.text)

                    for act in app:
                        f.write(act.text)
                else:
                    f.write(t.text)

        if self.args.inplace:
            with open(unit.filename, "w") as f:
                write_all(f)
        else:
            header = "Output for " + unit.filename
            print(header)
            print("=" * len(header))
            print()

            write_all(sys.stdout)


    def add_action(self, token, action):
        self.actions[token].append(action)

    def preprocess(self, unit):
        pass
