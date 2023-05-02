from __future__ import annotations

from typing import Callable, List, Tuple, cast

from langkit.utils.colors import printcol, Colors

import liblkqllang as L

from pretty_printer import PrettyPrinter, ad, ai, hl, sl, fi, fd, ob, cb

def print_header(s: str) -> None:
    printcol(f"{s}\n{'=' * len(s)}\n", Colors.BLUE)


def get_comments_before(n: L.LkqlNode) -> Tuple[List[str], bool]:
    t_start = cast(L.Token, n.token_start)
    f = t_start.previous
    ret: List[str] = []
    number_of_newlines = 0
    while f and f.is_trivia:
        if f.kind == "Whitespace" and "\n" in f.text and not ret:
            number_of_newlines += 1

        if f.kind == "Comment":
            ret.append(f.text)

        f = f.previous

    return list(reversed(ret)), number_of_newlines > 1


def get_trivia_after(n: L.LkqlNode) -> List[L.Token]:
    t_end = cast(L.Token, n.token_end)
    f = t_end.next
    ret = []
    while f and f.is_trivia:
        ret.append(f)
        f = f.next

    return ret


def pretty_print_decl(n: L.LkqlNode, pp: PrettyPrinter) -> None:
    r = lambda nn: pretty_print(nn, pp)

    if isinstance(n, L.FunDecl):
        pp.add(f"fun {n.f_name.text}")
        r(n.f_fun_expr)

    elif isinstance(n, L.ParameterDecl):
        pp.add(n.f_param_identifier.text)
        d = n.f_default_expr
        if d:
            pp.add("=")
            r(d)

    elif isinstance(n, L.ValDecl):
        pp.add(f"val {n.f_identifier.text} = ")
        r(n.f_value)

    else:
        pp.add(f"<{type(n).__name__}>")


already_processed_comments = set()


def print_comments(
    fn: Callable[[L.LkqlNode, PrettyPrinter], None]
) -> Callable[[L.LkqlNode, PrettyPrinter], None]:

    def print_comments(n: L.LkqlNode, pp: PrettyPrinter) -> None:
        comments, has_nl = get_comments_before(n)

        if comments and comments[0] not in already_processed_comments:
            for comment in comments:
                already_processed_comments.add(comment)
                pp.add(comment, hl)
            if has_nl:
                pp.add(hl)

        fn(n, pp)

    return print_comments


@print_comments
def pretty_print(n: L.LkqlNode, pp: PrettyPrinter) -> None:
    r = lambda nn: pretty_print(nn, pp)

    if isinstance(n, L.TopLevelList):
        for i, el in enumerate(n):
            r(el)
            if i < len(n) - 1:
                pp.add(hl, hl)

    elif isinstance(n, L.Declaration):
        if n.f_annotation:
            r(n.f_annotation)
            pp.add(hl)
        pretty_print_decl(n, pp)

    elif isinstance(n, L.DeclAnnotation):

        pp.add(f"@{n.f_name.text}")
        if len(n.f_arguments):
            pp.add("(")
            with pp.aligned_box(-1):
                r(n.f_arguments)
            pp.add(")")

    elif isinstance(n, L.BlockStringLiteral):
        for el in n.f_docs:
            pp.add(el.text, hl)

    elif isinstance(n, L.BaseFunction):
        pp.add("(")
        with pp.aligned_box():
            with pp.aligned_box():
                r(n.f_parameters)
        pp.add(") = ")

        if n.f_doc_node:
            pp.add(ob, fi, hl)
            r(n.f_doc_node)
            pp.add(fd, cb)

        # TODAY: Transform into predicate
        is_block_expr = n.f_body_expr.is_a(L.BlockExpr, L.ListLiteral)

        if not is_block_expr:
            pp.add(ob, fi, sl)

        r(n.f_body_expr)

        if not is_block_expr:
            pp.add(fd, cb)

    elif isinstance(n, L.DetailPattern):
        r(n.f_pattern_value)

    elif isinstance(n, (L.ParameterDeclList, L.ArgList)):
        for i, par in enumerate(n):
            r(par)
            if i < len(n) - 1:
                pp.add(", ", sl)

    elif isinstance(n, L.ListComprehension):
        pp.add("[]")

    elif isinstance(n, L.BlockExpr):
        pp.add("{", fi, hl)

        for el in n.f_body:
            r(el)
            pp.add(";", hl)

            if isinstance(cast(L.BlockBodyDecl, el).f_decl, L.FunDecl):
                pp.add(hl)

        r(n.f_expr)
        pp.add(fd, hl, "}")

    elif isinstance(n, L.BlockBodyDecl):
        r(n.f_decl)

    elif isinstance(n, (L.Identifier, L.StringLiteral, L.BoolLiteral,
                        L.IntegerLiteral, L.NullLiteral)):
        pp.add(n.text)

    elif isinstance(n, L.FunCall):
        r(n.f_name)

        if len(n.f_arguments):
            pp.add("(", fi, ob, sl)
            r(n.f_arguments)
            pp.add(fd, sl, cb, ")")
        else:
            pp.add("()")

    elif isinstance (n, L.ExprArg):
        r(n.f_value_expr)

    elif isinstance (n, L.NamedArg):
        r(n.f_arg_name)
        pp.add("=")
        r(n.f_value_expr)

    elif isinstance(n, L.IsClause):
        r(n.f_node_expr)
        pp.add(" is ")
        r(n.f_pattern)

    elif isinstance(n, L.NodeKindPattern):
        pp.add(n.f_kind_name.text)

    elif isinstance(n, L.ExtendedNodePattern):
        r(n.f_node_pattern)
        pp.add("(", fi, ob, sl)
        for detail in n.f_details:
            r(detail)
        pp.add(fd, sl, cb, ")")

    elif isinstance(n, L.NodePatternField):
        r(n.f_identifier)
        pp.add(" is ")
        r(n.f_expected_value)

    elif isinstance(n, L.ListLiteral):
        pp.add("[", fi, ob, sl)
        for i, el in enumerate(n.f_exprs):
            r(el)
            if i < len(n.f_exprs) - 1:
                pp.add(", ")
                pp.add(sl)
        pp.add(fd, sl, cb, "]")

    elif isinstance(n, L.FilteredPattern):
        r(n.f_pattern)
        pp.add(" ", sl, "when", " ")
        r(n.f_predicate)

    elif isinstance(n, L.NodePatternProperty):
        r(n.f_call)
        pp.add(" is ")
        r(n.f_expected_value)

    elif isinstance(n, L.ParenPattern):
        pp.add("(")
        r(n.f_pattern)
        pp.add(")")

    elif isinstance(n, L.SafeAccess):
        r(n.f_receiver)
        pp.add("?.")
        r(n.f_member)

    elif isinstance(n, L.DotAccess):
        # TODO: Maybe break nested dot accesses
        r(n.f_receiver)
        if not n.parent.is_a(L.DotAccess):
            pp.start_aligned_box(1)
        pp.add(sl)
        pp.add(".")
        r(n.f_member)
        if not n.parent.is_a(L.DotAccess):
            pp.close_aligned_box()

    elif isinstance(n, L.OrPattern):

        r(n.f_left)
        pp.add(" or ")
        r(n.f_right)

    elif isinstance(n, L.Import):
        pp.add("import ")
        r(n.f_name)

    elif isinstance(n, L.Match):
        with pp.aligned_box():
            pp.add("match ")
            r(n.f_matched_val)
            pp.add(hl)
            for i, arm in enumerate(n.f_arms):
                pp.add("| ", ob, fi)
                r(arm.f_pattern)
                pp.add (" => ", sl)
                r(arm.f_expr)

                pp.add(fd, cb)
                if i < len(n.f_arms) - 1:
                    pp.add(hl)

    elif isinstance(n, L.BindingPattern):
        r(n.f_binding)
        pp.add("@")
        r(n.f_value_pattern)

    elif isinstance(n, L.IfThenElse):

        # We create an align box, except in the case where this if is
        # directly nested in an other if expression, in which case we want
        # to align every sub-if-expr on the top level one
        if not n.parent.is_a(L.IfThenElse):
            pp.start_aligned_box()

        pp.add("if ", ob)
        r(n.f_condition)
        pp.add(" ", sl, "then ")
        r(n.f_then_expr)
        pp.add(cb, " ", sl, "else ")
        r(n.f_else_expr)

        if not n.parent.is_a(L.IfThenElse):
            pp.close_aligned_box()

    elif isinstance(n, L.UniversalPattern):
        pp.add("*")

    elif isinstance(n, L.BinOp):
        with pp.aligned_box():
            with pp.plain_box():
                r(n.f_left)
            pp.add(sl, " " + n.f_op.text + " ")
            with pp.plain_box():
                r(n.f_right)

    elif isinstance(n, L.UnOp):
        pp.add(n.f_op.text + " ")
        r(n.f_operand)

    elif isinstance(n, L.Indexing):
        r(n.f_collection_expr)
        pp.add("[")
        r(n.f_index_expr)
        pp.add("]")
    else:
        pp.add(f"<{type(n).__name__}>")


class App(L.App):
    def process_unit(self, unit: L.AnalysisUnit) -> None:
        if self.args.debug:
            unit.root.dump()

        print_header("Original")
        print(unit.text)

        print_header("Pretty printed")
        pp = PrettyPrinter(debug=self.args.debug)
        pretty_print(unit.root, pp)
        print(pp.pretty_print())

    def add_arguments(self):
        self.parser.add_argument("-D", "--debug", action="store_true")

        self.parser.add_argument("-o", "--output", type=str)

if __name__ == "__main__":
    App.run()
