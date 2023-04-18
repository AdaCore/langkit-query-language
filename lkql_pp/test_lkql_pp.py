from typing import List

from lkql_pp import PPFragment, PrettyPrinter, txt, hl, fi, fd
from inspect import cleandoc


def clean_expected_out(strn: str) -> str:
    return cleandoc(strn) + "\n"


def do_test(actions: List[PPFragment], expected: str) -> None:
    pp = PrettyPrinter()
    pp.add(*actions)
    assert pp.pretty_print() == clean_expected_out(expected)

def test_pp_hello_world():
    do_test([txt("Hello world")], "Hello world")


def test_pp_hl():
    do_test(
        [
            txt("Hello world"), hl,
            txt("Pouet"),
        ],

        """
        Hello world
        Pouet
        """
    )


def test_pp_indent_basic():
    do_test(
        [
            txt("Hello world"),
            fi, hl,
            txt("Pouet"), hl,
            txt("Pouet2"), fd, hl,
            txt("After")
        ],

        """
        Hello world
            Pouet
            Pouet2
        After
        """
    )
