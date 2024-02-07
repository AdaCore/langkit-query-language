# -*- coding: utf-8 -*-
#
# GNATcheck build configuration file

# -- Project information -----------------------------------------------------

from os import path as P
import sys
import time

# Add own dir path
own_dir_path = P.dirname(P.realpath(__file__))
sys.path.append(own_dir_path)

# Add lkql user_manual path into the Python path, so that we have access to the
# lkql pygments lexer module.
lkql_user_manual_path = P.join(
    P.dirname(P.dirname(P.dirname(P.dirname(P.realpath(__file__))))),
    "user_manual", "source"
)

sys.path.append(lkql_user_manual_path)

import ada_pygments
import latex_elements
import lkql_lexer

# -- General configuration ---------------------------------------------------

from sphinx.highlighting import lexers

lexers['lkql'] = lkql_lexer.LKQLPygmentsLexer()
lexers['ada'] = ada_pygments.AdaLexer()
lexers['gpr'] = ada_pygments.GNATProjectLexer()

# -- General configuration ---------------------------------------------------

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".

# TODO: Add back the lkql syntax check, factor it from LKQL's user manual
extensions = ['sphinx.ext.viewcode', 'lkql_doc_class']
templates_path = ['_templates']
source_suffix = '.rst'
master_doc = 'gnatcheck_rm'

# General information about the project.
project = 'GNATcheck Reference Manual'
copyright = u'2008-%s, AdaCore' % time.strftime('%Y')
author = 'AdaCore'


def get_version():
    for line in open("../../src/gnatcheck-options.ads").readlines():
        if line.lstrip().startswith('Gnatcheck_Version'):
            return line[line.find('"') + 1:line.rfind('"')]
    raise Exception("Could not find the current version of GNATcheck")


version = get_version()
release = version
doc_name = 'gnatcheck_rm'

pygments_style = 'sphinx'

html_theme = 'sphinx_rtd_theme'
if P.isfile('favicon.ico'):
    html_favicon = 'favicon.ico'

html_logo = 'adacore-logo-white.png'
html_theme_options = {
    "style_nav_header_background": "#12284c",
}

latex_additional_files = ['gnat.sty']

latex_elements = {
    'preamble': latex_elements.TOC_DEPTH +
    latex_elements.PAGE_BLANK +
    latex_elements.TOC_CMD +
    latex_elements.LATEX_HYPHEN +
    latex_elements.doc_settings(project, get_version()),
    'tableofcontents': latex_elements.TOC}

latex_documents = [
    (master_doc, '%s.tex' % doc_name, project, u'AdaCore', 'manual')]

texinfo_documents = [
    (master_doc, doc_name, project,
     u'AdaCore', doc_name, doc_name, '')]
