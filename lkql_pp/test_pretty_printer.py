from __future__ import annotations

from typing import List

import pytest

from pretty_printer import (
    PPFragment, PrettyPrinter, ad, ai, hl, fi, fd, sl, ob,
    cb
)
from inspect import cleandoc


def clean_expected_out(strn: str) -> str:
    return cleandoc(strn) + "\n"


class DisplayStr(str):
    """
    String subclass used solely to have readable test output, since pytest
    uses the __repr__ to show variations when comparing outputs.
    """
    def __repr__(self):
        return str(self)


def do_test(actions: List[PPFragment | str], expected: str) -> None:
    pp = PrettyPrinter()
    pp.add(*actions)
    assert (
        DisplayStr(pp.pretty_print())
        == DisplayStr(clean_expected_out(expected))
    )


def test_pp_hello_world():
    do_test(
        ["Hello world"], "Hello world"
    )


def test_pp_hl():
    do_test(
        [
            "Hello world", hl,
            "Pouet",
        ],

        """
        Hello world
        Pouet
        """
    )


def test_pp_indent_basic():
    do_test(
        [
            "Hello world",
            fi, hl,
            "Pouet", hl,
            "Pouet2", fd, hl,
            "After"
        ],

        """
        Hello world
            Pouet
            Pouet2
        After
        """
    )


def test_pp_boxes_basic_nobreak():
    do_test(
        [
            "Hello world(",
            fi, sl,
            "Pouet, ", sl,
            "Pouet2, ", sl,
            "Pouet3", sl,
            fd, sl,
            ");", hl,
            "After"
        ],

        """
        Hello world(Pouet, Pouet2, Pouet3);
        After
        """
    )


def test_pp_boxes_basic_break():
    do_test(
        [
            "Hello world(", ob,
            fi, sl,
            "blablablabla (bloblo), ", sl,
            "blu_blu_blu (bloblo), ", sl,
            "pouet_pouet_pouet (coin)", fd, sl, cb,
            ");", hl,
            "After"
        ],

        """
        Hello world(
            blablablabla (bloblo),
            blu_blu_blu (bloblo),
            pouet_pouet_pouet (coin)
        );
        After
        """
    )


def test_pp_boxes_basic_break():
    do_test(
        [
            "Hello world(", ob,
            fi, sl,
            "blablablablabla (bloblo), ", sl,
            "blublublublublub (bloblo), ", sl,
            "pouetpouetpouetpou (coin)", fd, sl, cb,
            ");", hl,
            "Hello world(", ob,
            fi, sl,
            "blablablabla (bloblo), ", sl,
            "blu_blu_blu (bloblo), ", sl,
            "pouet_pouet_pouet (coin)", fd, sl, cb,
            ");", hl,
            "After"
        ],

        """
        Hello world(
            blablablablabla (bloblo),
            blublublublublub (bloblo),
            pouetpouetpouetpou (coin)
        );
        Hello world(
            blablablabla (bloblo),
            blu_blu_blu (bloblo),
            pouet_pouet_pouet (coin)
        );
        After
        """
    )


def test_pp_boxes_basic_break():
    do_test(
        [
            "Hello world(", ob,
            fi, sl,
            "bla(bloblo), ", sl,
            "blu(bloblo), ", sl,
            "pou(coin)", fd, sl, cb,
            ");", hl,
            "Hello world(", ob,
            fi, sl,
            "blablablabla (bloblo), ", sl,
            "blu_blu_blu (bloblo), ", sl,
            "pouet_pouet_pouet (coin)", fd, sl, cb,
            ");", hl,
            "After"
        ],

        """
        Hello world(bla(bloblo), blu(bloblo), pou(coin));
        Hello world(
            blablablabla (bloblo),
            blu_blu_blu (bloblo),
            pouet_pouet_pouet (coin)
        );
        After
        """
    )


def test_pp_boxes_nested():
    do_test(
        [
            "Hello world(", ob,
            fi, sl,

            "bla(", ob, fi, sl,
            "foo_the_bar (12, 15), ", sl,
            "bar_the_foo (12, 15), ", sl,
            "bar_the_foo (12, 15),", sl,

            "bar_the_foo (", ob, fi, sl,
            "foo_the_bar (12, 15), ", sl,
            "bar_the_foo (12, 15), ", sl,
            "bar_the_foo (12, 15)", fd, sl, cb, "),",

            fd, sl, cb, "),", sl,

            "blu(bloblo), ", sl,
            "pou(coin)", fd, sl, cb,
            ");", hl,
            "Hello world(", ob,
            fi, sl,
            "blablablabla (bloblo), ", sl,
            "blu_blu_blu (bloblo), ", sl,
            "pouet_pouet_pouet (coin)", fd, sl, cb,
            ");", hl,
            "After"
        ],

        """
        Hello world(
            bla(
                foo_the_bar (12, 15),
                bar_the_foo (12, 15),
                bar_the_foo (12, 15),
                bar_the_foo (
                    foo_the_bar (12, 15),
                    bar_the_foo (12, 15),
                    bar_the_foo (12, 15)
                ),
            ),
            blu(bloblo),
            pou(coin)
        );
        Hello world(
            blablablabla (bloblo),
            blu_blu_blu (bloblo),
            pouet_pouet_pouet (coin)
        );
        After
        """
    )


def test_aligned_indent():
    do_test(
        [
            "Hello world", "(", ai(-1),
            "bla, ", hl,
            "bla, ", hl,
            "bla", ");", ad, hl,
            "After"
        ],

        """
        Hello world(bla,
                    bla,
                    bla);
        After
        """
    )


def test_aligned_indent_plus_box():
    do_test(
        [
            "Hello world(", ob,
            fi, sl,

            "bla(", ob, fi, sl,
            "foo_the_bar (12, 15), ", sl,
            "bar_the_foo (12, 15), ", sl,
            "bar_the_foo (12, 15),", sl,

            "bar_the_foo (", ob, ai(-1),
            "foo_the_bar (12, 15), ", sl,
            "bar_the_foo (12, 15), ", sl,
            "bar_the_foo (12, 15)", ad, cb, "),",

            fd, sl, cb, "),", sl,

            "blu(bloblo), ", sl,
            "pou(coin)", fd, sl, cb,
            ");"
        ],

        """
        Hello world(
            bla(
                foo_the_bar (12, 15),
                bar_the_foo (12, 15),
                bar_the_foo (12, 15),
                bar_the_foo (foo_the_bar (12, 15),
                             bar_the_foo (12, 15),
                             bar_the_foo (12, 15)),
            ),
            blu(bloblo),
            pou(coin)
        );
        """
    )
