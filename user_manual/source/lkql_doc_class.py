"""
The aim of this directive is to document inline which classes of the LKQL
hierarchy are already documented, in reference_manual.rst, and get a report of
which classes remain to be documented.
"""


from collections import defaultdict
from docutils import nodes
from functools import lru_cache

from sphinx.util.docutils import SphinxDirective

try:
    import liblkqllang
    LKQL_CLASSES = [
        v for _, v in liblkqllang.__dict__.items()
        if (type(v) == type
            and issubclass(v, liblkqllang.LkqlNode))
    ]

except ImportError:
    liblkqllang = None
    LKQL_CLASSES = []

import traceback


@lru_cache
def lkql_cls_subclasses():
    """
    Return a dict of class to direct subclasses.
    """
    res = defaultdict(list)
    for cls in LKQL_CLASSES:
        if cls != liblkqllang.LkqlNode:
            res[cls.__base__].append(cls)
    return res


@lru_cache(maxsize=None)
def is_class_documented(lkql_class):
    """
    Helper function: whether a class is documented, taking inheritance into
    account (if all subclasses of a class are documented, then the class is
    documented).
    """
    if issubclass(lkql_class, liblkqllang.LkqlNodeBaseList):
        return True
    subclasses = lkql_cls_subclasses()[lkql_class]
    return (
        getattr(lkql_class, "documented", False)
        or (len(subclasses) > 0
            and all(is_class_documented(subcls)
                    for subcls in lkql_cls_subclasses()[lkql_class]))
    )


class LkqlDocClassDirective(SphinxDirective):
    """
    Directive to be used to annotate documentation of an LKQL node.
    """

    has_content = False
    required_arguments = 1
    optional_arguments = 0

    def run(self):
        cls_name = self.arguments[0]

        if not hasattr(self.env, 'documented_classes'):
            self.env.documented_classes = []

        try:
            lkql_class = getattr(liblkqllang, cls_name)
            self.env.documented_classes.append(lkql_class)
            lkql_class.documented = True
        except AttributeError:
            raise self.warning(f"LKQL class not found: {cls_name}")

        return []


def process_lkql_classes_coverage(app, doctree, fromdocname):
    """
    Process the coverage of lkql classes in documentation. This will print
    warnings for every non-documented class.
    """
    try:
        for cls in LKQL_CLASSES:
            if not is_class_documented(cls):
                doctree.reporter.warning(f"Class not documented: {cls}")
    except Exception as e:
        traceback.print_exception(type(e), e, e.__traceback__)


def check_lkql_code(app, doctree, fromdocname):
    """
    Visit code blocks and check if they're valid LKQL code.
    """

    class MyVisitor(nodes.NodeVisitor):

        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)
            self.lkql_context = liblkqllang.AnalysisContext()
            self.ipython = True

        def visit_literal_block(self, node) -> None:
            """
            Visit code blocks and check if they're valid LKQL code.
            """

            # Don't check non lkql blocks
            if node.attributes['language'].lower().strip() != 'lkql':
                return

            text = node[0].astext()
            unit = self.lkql_context.get_from_buffer('<buffer>', text)
            if unit.diagnostics:
                for diag in unit.diagnostics:
                    doctree.reporter.warning(
                        f"LKQL syntax error: {diag.message}",
                        line=node.line + 1 + diag.sloc_range.start.line
                    )

        def unknown_visit(self, node) -> None:
            """
            Visit every other node type. Mandatory in docutils visitors
            apparently.
            """
            pass

    doctree.walk(MyVisitor(doctree))


def setup(app):
    if liblkqllang:
        app.add_directive('lkql_doc_class', LkqlDocClassDirective)
        app.connect('doctree-resolved', process_lkql_classes_coverage)
        app.connect('doctree-resolved', check_lkql_code)

    return {
        'version': '0.1',
        'parallel_read_safe': True,
        'parallel_write_safe': True,
    }
