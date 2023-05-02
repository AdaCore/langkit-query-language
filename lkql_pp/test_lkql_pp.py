from __future__ import annotations

import pathlib
from contextlib import redirect_stdout
from io import StringIO

from lkql_pp import PrettyPrinter, pretty_print, App
from test_pretty_printer import DisplayStr, clean_expected_out

import liblkqllang as L


def do_test(lkql_source: str, expected: str):
    ctx = L.AnalysisContext()
    unit = ctx.get_from_buffer("<input>", clean_expected_out(lkql_source))
    pp = PrettyPrinter()
    pretty_print(unit.root, pp)
    result = pp.pretty_print()
    expected = clean_expected_out(expected)
    assert DisplayStr(result) == DisplayStr(expected)


def do_test_identical(lkql_source: str):
    do_test(lkql_source, lkql_source)


def test_lkql_1():
    do_test_identical(
        """
        @memoized
        fun hello(dummy) = {
            val dummy2 = dummy;
            "hello"
        }

        @check
        fun do_bug(node) = {
            val hell = hello("");
            false
        }
        """
    )


def test_lkql_comments_newlines_1():
    do_test_identical(
        """
        # A comment

        val a = "hello"
        """
    )


def test_lkql_comments_newlines_2():
    do_test_identical(
        """
        # Another comment
        val b = "hi"
        """
    )


def test_simple_checker():
    do_test_identical(
        """
        # Flag abort statements.

        @check(message="abort statement", category="Feature")
        fun abort_statements(node) = node is AbortStmt
        """
    )


def test_annotation_aligned_indent():
    do_test_identical(
        """
        @check(message="declaration of abstract type",
               help="abstract types",
               category="Feature")
        fun abstract_type_declarations(node) = true
        """
    )


def test_filtered_pattern_aligned_indent():
    do_test_identical(
        """
        fun abstract_type_declarations(node) =
            node is (RecordTypeDef or DerivedTypeDef or PrivateTypeDef)
            when node.f_has_abstract.p_as_bool()
        """
    )


def test_import():
    do_test_identical(
        """
        import foo
        
        val a = foo.bar()
        """
    )


def test_match():
    """
    Test the match expression, as well as a bunch of patterns
    """
    do_test_identical(
        """
        fun a() = match n
                  | n@Node => foo()
                  | OtherNode => pouet()
                  | * => pouet()
        """
    )


def test_fixed_indent_and():
    do_test_identical(
        """
        fun access_to_local_objects(node) =
            node is AttributeRef
            when node.f_attribute.p_name_is("Access")
                 and denotes_local_object(node.f_prefix)
        """
    )


def test_named_params():
    """
    Check that named parameters and named parameter declarations work as
    expected.
    """
    do_test_identical(
        """
        fun add(a=12, b=12) = a + b
        
        val c = add(a=12, b=12)
        """
    )


def test_fundecl_inside_block():
    """
    Check that there is a blank line after a fun decl inside a block.
    """
    do_test_identical(
        """
        fun foo() = {
            fun bar() = 12;
            
            bar()
        }
        """
    )


def test_unop():
    """
    Test unary operators
    """

    do_test_identical(
        """
        val a = not b
        """
    )


def test_app(tmp_path: pathlib.Path):
    """
    Test the main lkql_pp App
    """
    file_str = StringIO()

    with redirect_stdout(file_str):
        tmp_file = tmp_path / "test.lkql"
        tmp_file.write_text("fun a() = 12")
        App.run([str(tmp_file.absolute())])

    assert (file_str.getvalue() == clean_expected_out("""
        Original
        ========

        fun a() = 12
        Pretty printed
        ==============

        fun a() = 12
    """) + '\n')


def test_if_then_else():
    do_test_identical(
        """
        fun pouet() =
            if condition_1() then action()
            else if very_long_long_long_condition_2() and other_condition()
            then action()
            else if prouet() then no()
            else hello()
        """
    )


def test_indexing():
    do_test_identical(
        """
        fun pouet() = a[12]
        """
    )


def test_indexing():
    do_test_identical(
        """
        fun pouet() = [
            "hello",
            "cruel",
            "world",
            "how",
            "are",
            "you",
            "doing",
            "today",
            "mate",
            "dude",
            "?"
        ]
        """
    )
