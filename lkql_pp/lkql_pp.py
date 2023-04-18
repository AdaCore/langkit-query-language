from dataclasses import dataclass
from functools import cache, reduce
from io import StringIO
from typing import Any, Callable, List, Set, Tuple, Type, cast

from langkit.dsl_unparse import pp, sf
from langkit.utils.colors import printcol, Colors
import liblkqllang as L


class PPFragment:
    pass


class HardLineBreak(PPFragment):
    pass


class SoftLineBreak(PPFragment):
    needs_split: bool = False


class OpenBox(PPFragment):
    split: bool = False


class CloseBox(PPFragment):
    pass


class IndentAction(PPFragment):
    pass


class FixedIndent(IndentAction):
    pass


class FixedDedent(IndentAction):
    pass


class AlignedIndent(IndentAction):
    pass


class AlignedDedent(IndentAction):
    pass


@dataclass
class Text(PPFragment):
    data: str


hl = HardLineBreak()
sl = SoftLineBreak()
ob = OpenBox()
cb = CloseBox()
fi = FixedIndent()
fd = FixedDedent()
ai = AlignedIndent()
ad = AlignedDedent()
txt = Text


def check_freeze(method: Any) -> Any:

    def internal(pretty_printer: 'PrettyPrinter',
                 *args: Any,
                 **kwargs: Any) -> None:
        if pretty_printer.frozen:
            raise Exception("Cannot append fragments to a frozen "
                            "pretty-printer")
        method(pretty_printer, *args, **kwargs)

    return internal


@dataclass
class Line:
    indent: int
    fragments: List[PPFragment]

    def remove_pred(self, pred: Callable[[PPFragment], bool]) -> None:
        self.fragments = [f for f in self.fragments if pred(f)]

    def remove(self, kls: Type[PPFragment]) -> None:
        self.remove_pred(lambda f: not isinstance(f, kls))

    @cache
    def text_fragments(self):
        return [
            cast(Text, f).data for f in self.fragments if isinstance(f, Text)
        ]

    def length(self):
        """
        Returns the length of this line
        """
        return reduce(lambda l, i: l + len(i), self.text_fragments())

    def render(self) -> str:
        return " " * self.indent + "".join(self.text_fragments())

    def split_on(self, frag):
        for i, f in enumerate(self.fragments):
            if frag == f:
                return (
                    Line(self.indent, self.fragments[0:i]),
                    Line(self.indent, self.fragments[i + 1:])
                )
        return (None, None)


@dataclass
class Box:
    breaks: List[SoftLineBreak]
    needs_split: bool = False


class PrettyPrinter:

    def __init__(self, indent_step: int = 4, line_size: int = 80):
        self.indent_step = indent_step
        self.line_size = line_size
        self.fragments: List[PPFragment] = []
        self.frozen = False
        self.lines: List[Line] = []

    def pre(self) -> None:
        pass


    def transform_line_breaks(self):
        pass

    def prepare(self) -> None:
        self.frozen = True

        cur: List[PPFragment] = []

        # Fill up the data table, split in lines according to hard line breaks
        for frag in self.fragments:
            # Split on hard line break
            if isinstance(frag, HardLineBreak):
                self.lines.append(Line(0, cur))
                cur = []
            # Instantiate soft line breaks (needed by the box line breaking
            # algorithm)
            elif isinstance(frag, SoftLineBreak):
                cur.append(SoftLineBreak())
            else:
                cur.append(frag)

        # Append the last line
        # TODO: Do we really want to do that? Or should the user insert a
        #  hard line break ?
        self.lines.append(Line(0, cur))

    def compute_indent(self) -> None:
        current_indent = 0
        for line in self.lines:
            line.indent = current_indent
            for i, fragment in enumerate(line.fragments):
                if isinstance(fragment, FixedIndent):
                    current_indent += self.indent_step
                elif isinstance(fragment, FixedDedent):
                    current_indent -= self.indent_step
                elif isinstance(fragment, (AlignedIndent, AlignedDedent)):
                    raise NotImplementedError(
                        "Aligned indentation not yet implemented"
                    )

    def compute_boxes(self) -> None:
        top_level_boxes: List[Box]
        needs_splitting: bool = False
        level: int = 0
        current_box = None

        for line in self.lines:
            for i, fragment in enumerate(line.fragments):
                if isinstance(fragment, OpenBox) and not fragment.split:
                    if level == 0:
                        current_box = Box([])
                        top_level_boxes.append(current_box)
                    level += 1

                elif isinstance(fragment, SoftLineBreak) and level == 1 and \
                        current_box:
                    current_box.breaks.append(fragment)

                elif isinstance(fragment, CloseBox):
                    level -= 1
                    if level == 0:
                        current_box = None

            if line.length() > self.line_size:
                # Split
                needs_splitting = True

        while needs_splitting:
            box = boxes_stack[0]
            del boxes_stack[0]

            for brk in box.breaks:
                brk.needs_split = True

            self.split_soft_line_breaks()

            needs_splitting = False





    def pretty_print(self) -> str:
        """
        Pretty printing DSL

        TODO: Document and share with langkit
        """

        self.pre()
        self.prepare()
        self.compute_indent()

        file_str = StringIO()
        for line in self.lines:
            file_str.write(line.render() + "\n")
        return file_str.getvalue()

    def add(self, *actions: PPFragment):
        self.fragments.extend(actions)


def get_comments_before(n: L.LkqlNode) -> Tuple[List[str], bool]:
    t_start = cast(L.Token, n.token_start)
    f = t_start.previous
    ret = []
    has_newlines_before = False
    while f and f.is_trivia:
        if f.kind == "Whitespace" and "\n" in f.text and not ret:
            has_newlines_before = True

        if f.kind == "Comment":
            ret.append(f.text)

        f = f.previous

    return list(reversed(ret)), has_newlines_before


def get_trivia_after(n: L.LkqlNode) -> List[L.Token]:
    t_end = cast(n.token_end, L.Token)
    f = t_end.next
    ret = []
    while f and f.is_trivia:
        ret.append(f)
        f = f.next

    return ret


def pretty_print_decl(n: L.LkqlNode) -> str:
    r = pretty_print
    if isinstance(n, L.FunDecl):
        return f"fun {n.f_name.text}{r(n.f_fun_expr)}"
    elif isinstance(n, L.ParameterDecl):
        d = n.f_default_expr
        if d:
            return f"{n.f_param_identifier.text} = {r(d)}"
        else:
            return f"{n.f_param_identifier.text}"
    elif isinstance(n, L.ValDecl):
        return f"val {n.f_identifier.text} = {r(n.f_value)}"

    else:
        return f"<{type(n).__name__}>"


already_processed_comments = set()

def print_comments(
    fn: Callable[[L.LkqlNode], str]
) -> Callable[[L.LkqlNode], str]:

    def print_comments(n: L.LkqlNode) -> str:
        ret = fn(n)
        comments, has_nl = get_comments_before(n)

        if comments and comments[0] not in already_processed_comments:
            for comment in comments:
                already_processed_comments.add(comment)

            return f"{'$hl'.join(comments)}{'$hl' if has_nl else ''}$hl{ret}"
        else:
            return ret

    return print_comments


@print_comments
def pretty_print(n: L.LkqlNode) -> str:
    r = pretty_print

    if isinstance(n, L.TopLevelList):
        return sf("""
        % for el in n:
        ${r(el)}$hl$hl
        % endfor
        """)

    elif isinstance(n, L.Declaration):
        if n.f_annotation:
            return sf("""
            ${r(n.f_annotation)}$hl
            ${pretty_print_decl(n)}
            """)
        else:
            return pretty_print_decl(n)

    elif isinstance(n, L.DeclAnnotation):

        if n.f_arguments:
            return f"@{n.f_name.text}({r(n.f_arguments)})"
        else:
            return f"@{n.f_name.text}"

    elif isinstance(n, L.BaseFunction):
        return (
            f"({r(n.f_parameters)}) = {r(n.f_body_expr)}"
        )

    elif isinstance(n, (L.ParameterDeclList, L.ArgList)):
        return ", ".join([r(p) for p in n])

    elif isinstance(n, L.BlockExpr):
        return sf("""{$i$hl
        % for el in n.f_body:
        ${r(el)}$hl
        % endfor
        ${r(n.f_expr)}$hl
        $d}
        """)

    elif isinstance(n, L.BlockBodyDecl):
        return r(n.f_decl)

    elif isinstance(n, (L.Identifier, L.StringLiteral, L.BoolLiteral)):
        return n.text

    elif isinstance(n, L.FunCall):

        return f"{r(n.f_name)}({r(n.f_arguments)})"

    elif isinstance (n, L.ExprArg):
        return r(n.f_value_expr)

    elif isinstance (n, L.NamedArg):
        return f"{r(n.f_arg_name)}={r(n.f_value_expr)}"

    elif isinstance(n, L.IsClause):
        return f"{r(n.f_node_expr)} $slis {r(n.f_pattern)}"

    elif isinstance(n, L.NodeKindPattern):
        return n.f_kind_name.text

    else:
        return f"<{type(n).__name__}>"


class App(L.App):

    def process_unit(self, unit):

        def print_header(s: str) -> None:
            printcol(f"{s}\n{'=' * len(s)}\n", Colors.BLUE)

        print_header("Original")
        print(unit.text)

        print_header("Pretty printed")
        print(pp(pretty_print(unit.root)))


if __name__ == "__main__":
    App.run()
