from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass
from io import StringIO
from typing import (
    Any, Callable, Generator, List, Tuple, cast,
    Optional as Opt
)

from langkit.utils.colors import col, Colors


from pprint import pformat
from typing import Any

from pygments import highlight
from pygments.formatters import Terminal256Formatter
from pygments.lexers import PythonLexer


def pprint_color(obj: Any) -> None:
    """Pretty-print in color."""
    print(highlight(pformat(obj), PythonLexer(), Terminal256Formatter()), end="")


class PPFragment:
    pass


class HardLineBreak(PPFragment):
    def __repr__(self) -> str:
        return "hl"


class SoftLineBreak(PPFragment):
    needs_split: bool = False

    def __repr__(self) -> str:
        return "sl"


class OpenBox(PPFragment):
    split: bool = False

    def __repr__(self) -> str:
        if self.split:
            return "!ob"
        return "ob"


class CloseBox(PPFragment):
    split: bool = False

    def __repr__(self) -> str:
        if self.split:
            return "!cb"
        return "cb"


class IndentAction(PPFragment):
    pass


class FixedIndent(IndentAction):
    def __repr__(self) -> str:
        return "fi"


class FixedDedent(IndentAction):
    def __repr__(self) -> str:
        return "fd"


@dataclass
class AlignedIndent(IndentAction):
    token_index: int = -1

    def __repr__(self) -> str:
        return f"ai({self.token_index})"


class AlignedDedent(IndentAction):
    def __repr__(self) -> str:
        return "ad"


@dataclass
class Text(PPFragment):
    data: str

    def __repr__(self) -> str:
        return f"'{self.data}'"


hl = HardLineBreak()
sl = SoftLineBreak()
ob = OpenBox()
cb = CloseBox()
fi = FixedIndent()
fd = FixedDedent()
ai = AlignedIndent
ad = AlignedDedent()
txt = Text


@dataclass
class Line:
    indent: int
    fragments: List[PPFragment]
    _fragments_offsets: List[int]
    fragments_text: List[str]
    cached_length: int = -1

    @property
    def fragments_offsets(self):
        _ = self.text_fragments
        return self._fragments_offsets

    @property
    def text_fragments(self) -> List[str]:
        if not self.fragments_text:
            self._fragments_offsets.clear()
            current_offset = 0
            for f in self.fragments:

                if isinstance(f, Text):
                    self.fragments_text.append(cast(Text, f).data)
                    current_offset += len(self.fragments_text[-1])
                else:
                    # We append empty strings for non-text fragments,
                    # so that all the fragments* list have the same length
                    self.fragments_text.append("")

                self._fragments_offsets.append(current_offset)

        return self.fragments_text

    def length(self) -> int:
        """
        Returns the length of this line
        """
        if self.fragments_offsets:
            return self.fragments_offsets[-1] + self.indent
        else:
            return 0

    def render(self) -> str:
        return " " * self.indent + "".join(self.text_fragments).strip()

    def split_on(self, frag: PPFragment) -> Tuple['Line', 'Line']:
        for i, f in enumerate(self.fragments):
            if frag == f:
                return (
                    Line(self.indent, self.fragments[0:i], [], []),
                    Line(self.indent, self.fragments[i + 1:], [], [])
                )

        assert False, "Fragment not found in line"

    def __repr__(self) -> str:
        return f"Line({self.indent}, {self.fragments})"


@dataclass
class Box:
    breaks: List[SoftLineBreak]
    open_box: OpenBox
    close_box: CloseBox | None
    needs_split: bool = False
    start_line: int = -1
    end_line: int = -1


class PrettyPrinter:

    def __init__(self, indent_step: int = 4, line_size: int = 80, debug: bool = False):
        self.indent_step = indent_step
        self.line_size = line_size
        self.fragments: List[PPFragment] = []
        self.frozen = False
        self.lines: List[Line] = []
        self.debug = debug

    def start_aligned_box(self, index: int = 0):
        self.add(ob, ai(index))

    def close_aligned_box(self):
        self.add(ad, cb)

    def dump_lines(self, filtr=lambda _: True):
        for l in self.lines:
            res_stack = [[]]
            for f in l.fragments:
                if isinstance(f, OpenBox):
                    new_lst = []
                    res_stack[-1].append(new_lst)
                    res_stack.append(new_lst)
                elif isinstance(f, CloseBox):
                    res_stack.pop()
                else:
                    if not filtr(f):
                        res_stack[-1].append(f)
            pprint_color(res_stack[-1])

    @contextmanager
    def plain_box(self) -> Generator[None, None, None]:
        try:
            self.add(ob)
            yield
        finally:
            self.add(cb)

    @contextmanager
    def aligned_box(self, index: int = 0) -> Generator[None, None, None]:
        try:
            self.start_aligned_box(index)
            yield
        finally:
            self.close_aligned_box()

    def prepare(self) -> None:
        self.frozen = True

        cur: List[PPFragment] = []

        # Fill up the data table, split in lines according to hard line breaks
        for frag in self.fragments:
            # Split on hard line break
            if isinstance(frag, HardLineBreak):
                self.lines.append(Line(0, cur, [], []))
                cur = []
            # Instantiate soft line breaks (needed by the box line breaking
            # algorithm)
            elif isinstance(frag, SoftLineBreak):
                cur.append(SoftLineBreak())
            elif isinstance(frag, OpenBox):
                cur.append(OpenBox())
            elif isinstance(frag, CloseBox):
                cur.append(CloseBox())
            else:
                cur.append(frag)

        # Append the last line
        # TODO: Do we really want to do that? Or should the user insert a
        #  hard line break ?
        self.lines.append(Line(0, cur, [], []))

    def compute_indent(self) -> None:
        current_indent = 0

        # Used only when aligned indents are used
        # TODO: Is that sound ? Should we always use it ?
        indent_stack: List[int] = []
        for line in self.lines:
            line.indent = current_indent
            for i, f in enumerate(line.fragments):
                if isinstance(f, FixedIndent):
                    current_indent += self.indent_step

                elif isinstance(f, FixedDedent):
                    current_indent -= self.indent_step

                elif isinstance(f, AlignedIndent):
                    offset = line.fragments_offsets[i + f.token_index]
                    indent_stack.append(current_indent)
                    current_indent = line.indent + offset

                elif isinstance(f, AlignedDedent):
                    current_indent = indent_stack.pop()

    def compute_boxes(self) -> bool:
        """
        Go over boxes, see if their content needs to be broken down on soft
        line breaks.

        If it does, then break lines, and return true.
        """

        top_level_boxes: List[Box] = []
        level: int = 0
        cur_box: Opt[Box] = None
        did_reformat: bool = False

        def split_all(line: Line, new_lines: List[Line]) -> None:
            """
            Split ``line`` into several lines, putting the result into
            ``new_lines``.
            """
            for f in line.fragments:
                if isinstance(f, SoftLineBreak) and f.needs_split:
                    ll, nl = line.split_on(f)

                    new_lines.append(ll)
                    return split_all(nl, new_lines)
            new_lines.append(line)

        for i, line in enumerate(self.lines):

            # If cur_box was set an end_line on last iteration, set it back
            # to None
            if cur_box and cur_box.end_line != -1:
                cur_box = None

            for f in line.fragments:
                if isinstance(f, OpenBox) and not f.split:
                    if level == 0:
                        cur_box = Box([], start_line=i, open_box=f,
                                      close_box=None)
                        top_level_boxes.append(cur_box)
                    level += 1

                elif isinstance(f, SoftLineBreak) and level == 1 and cur_box:
                    cur_box.breaks.append(f)

                elif isinstance(f, CloseBox) and not f.split:
                    level -= 1
                    if level == 0:
                        assert cur_box is not None
                        cur_box.end_line = i
                        cur_box.close_box = f

            if line.length() > self.line_size:
                if cur_box:
                    cur_box.needs_split = True
                else:
                    print("WARNING: too long line that isn't splittable", line)

        for i, box in enumerate(top_level_boxes):
            if not box.needs_split:
                continue

            assert box.close_box, "Close box should be set at this point"
            box.open_box.split = True
            box.close_box.split = True
            did_reformat = True

            for b in box.breaks:
                b.needs_split = True

            new_lines: List[Line] = []

            for i, l in enumerate(self.lines[box.start_line:box.end_line + 1]):
                split_all(l, new_lines)

            self.lines[box.start_line:box.end_line + 1] = new_lines

            # Iterate on every box after ``box``, to adjust its start and
            # end lines.
            diff = len(new_lines) - (box.end_line - box.start_line + 1)
            for bx in top_level_boxes[i + 1:]:
                bx.start_line += diff
                bx.end_line += diff

        return did_reformat

    def _render(self) -> str:
        file_str = StringIO()
        for line in self.lines:
            file_str.write(line.render() + "\n")
        return file_str.getvalue()

    def pretty_print(self) -> str:
        """
        Pretty printing DSL

        TODO: Document and share with langkit
        """

        # First phase: prepare
        self.prepare()
        self.verify_boxes()

        if self.debug:
            print("# Debug output")
            self.dump_lines(lambda f: isinstance(f, IndentAction))
            print()

        # Second phase: iteratively compute indentation and line breaks until
        # there are no more lines to break anymore
        self.compute_indent()
        i = 0
        while self.compute_boxes():
            self.compute_indent()
            i += 1

        return self._render()

    def add(self, *actions: PPFragment | str) -> None:

        if self.frozen:
            raise Exception("Cannot append fragments to a frozen "
                            "pretty-printer")

        for act in actions:
            if isinstance(act, str):
                self.fragments.append(txt(act))
            else:
                self.fragments.append(act)

    def verify_boxes(self):
        ret = []
        level = 0
        for line in self.lines:
            for frag in line.fragments:
                if isinstance(frag, OpenBox):
                    if not frag.split:
                        level += 1
                    ret.append(frag)
                elif isinstance(frag, CloseBox):
                    if not frag.split:
                        level -= 1
                    ret.append(frag)

        assert level == 0, f"Boxes are not balanced!\n{ret}"
