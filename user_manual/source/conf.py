# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

import os
import sys
from pygments.lexer import RegexLexer, words
from pygments import token
from sphinx.highlighting import lexers

dir_path = os.path.dirname(os.path.realpath(__file__))
sys.path.append(dir_path)

project = 'LKQL'
copyright = '2020, Raphael Amiard'
author = 'Raphael Amiard'


class LKQLPygmentsLexer(RegexLexer):
    """
    Pygments lexer for LKQL
    """
    name = 'LKQL'
    filenames = ['*.lkql']

    tokens = {
        'root': [
            (words(('select', 'let', 'when', 'val', 'fun', 'selector',
                    'match', 'rec', 'skip', 'is', 'in', 'true', 'false',
                    'if', 'else', 'then', 'not', 'null'),
                   prefix=r'\b', suffix=r'\b'),
             token.Keyword),
            (r"#(.?)+", token.Comment),
            (r"(\-\>|=|\=\>|\<\=|\>\=|\=|\!\=|\+|\-|\*|\/|\&|"
             r"\@|\||\>|\<)", token.Operator),
            (r"\b(and|or|not)\b", token.Operator),
            (r"\{|\}|\(|\)|\[|\]|;|\.|,", token.Punctuation),
            (r"\"[^\"]*\"", token.String),
            (r'[0-9]+', token.Number),
            (r'_?[a-zA-Z][\w\']*', token.Name),
            (r'_', token.Name),
            (r'\n', token.Text),
            (r'[^\S\n]+', token.Text),
        ]
    }

lexers['lkql'] = LKQLPygmentsLexer()

# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = ['lkql_doc_class']

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = []

html_css_files = [
    'css/railroad.css'
]

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
if os.environ.get('USE_SPHINX_RTD_THEME'):
    import sphinx_rtd_theme
    html_theme = "sphinx_rtd_theme"
    html_theme_path = sphinx_rtd_theme.get_html_theme_path()
else:
    html_theme = 'alabaster'


# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']
