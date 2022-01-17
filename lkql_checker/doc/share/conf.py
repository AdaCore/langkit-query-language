# -*- coding: utf-8 -*-
#
# GNATcheck build configuration file

# -- Project information -----------------------------------------------------

import os
import sys
import time

from liblkqllang import LKQLPygmentsLexer

dir_path = os.path.dirname(os.path.realpath(__file__))
sys.path.append(dir_path)

import ada_pygments
import latex_elements

# -- General configuration ---------------------------------------------------

from sphinx.highlighting import lexers

lexers['lkql'] = LKQLPygmentsLexer()
lexers['ada'] = ada_pygments.AdaLexer()
lexers['gpr'] = ada_pygments.GNATProjectLexer()

# -- General configuration ---------------------------------------------------

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".

extensions = ['lkql_doc_class']
templates_path = ['_templates']
source_suffix = '.rst'
master_doc = 'gnatcheck_rm'

# General information about the project.
project = 'GNATcheck Reference Manual'
copyright = u'2008-%s, AdaCore' % time.strftime('%Y')
author = 'AdaCore'


def get_version():
    return '23.0w'


version = get_version()
release = get_version()
doc_name = 'gnatcheck_rm'

pygments_style = 'sphinx'

html_theme = 'sphinxdoc'
if os.path.isfile('adacore_transparent.png'):
    html_logo = 'adacore_transparent.png'
if os.path.isfile('favicon.ico'):
    html_favicon = 'favicon.ico'

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
