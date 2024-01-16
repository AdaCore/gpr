# -*- coding: utf-8 -*-
#
# GNAT build configuration file

import sys
import os
import time
import re

sys.path.append('.')

import ada_pygments
import latex_elements

# Some configuration values for the various documentation handled by
# this conf.py

DOCS = {
    'gprbuild_ug': {
        'title': u'GPR Tools User\'s Guide'}}
doc_name = 'gprbuild_ug'

# Then retrieve the source directory
root_source_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
gpr_version_spec = os.path.join(root_source_dir, '..', 'src', 'lib',
                                'gpr2-version.ads')
texi_fsf = True  # Set to False when FSF doc is switched to sphinx by default

numfig = True    # Allow automatic numbering of figures

with open(gpr_version_spec, 'r') as fd:
    gpr_version_content = fd.read()


def get_copyright():
    return u'2008-%s, Free Software Foundation' % time.strftime('%Y')


def get_gpr_version():
    m = re.search(r'Short_Value : ' +
                  r'constant String := "([^"]+)";',
                  gpr_version_content)
    if m:
        return m.group(1).strip()
    print('cannot find GPR version in ' + gpr_version_spec)
    return 'unknown'


# Exclude sources that are not part of the current documentation
exclude_patterns = []
for d in os.listdir(root_source_dir):
    if d not in ('share', doc_name, doc_name + '.rst'):
        exclude_patterns.append(d)
        print('ignoring %s' % d)

extensions = ['sphinx_rtd_theme']
templates_path = ['_templates']
source_suffix = '.rst'
master_doc = doc_name

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
# https://sphinx-rtd-theme.readthedocs.io/en/stable/configuring.html#theme-options
html_theme_options = {
    # Use AdaCore blue in the Table Of Content
    "style_nav_header_background": "#12284c",
}

html_static_path = ["_static"]

html_css_files = ["custom.css"]

# Search page using pagefind indexing
html_additional_pages = {
  'search': 'search.html', # relative to the _templates directory
}
# General information about the project.
project = DOCS[doc_name]['title']

copyright = get_copyright()

version = get_gpr_version()
# now = datetime.date.today()
# date = now.strftime('%Y%m%d')
# release = get_gpr_version() + ' (' + date + ')'
release = get_gpr_version()

pygments_style = None
html_theme = 'sphinx_rtd_theme'
html_logo = 'adacore-logo-white.png'
if os.path.isfile('favicon.ico'):
    html_favicon = 'favicon.ico'

latex_elements = {
    'preamble': latex_elements.TOC_DEPTH +
    latex_elements.PAGE_BLANK +
    latex_elements.TOC_CMD +
    latex_elements.LATEX_HYPHEN +
    latex_elements.doc_settings(DOCS[doc_name]['title'],
                                version),
    'tableofcontents': latex_elements.TOC}

latex_documents = [
    (master_doc, '%s.tex' % doc_name, project, u'AdaCore', 'manual')]

texinfo_documents = [
    (master_doc, doc_name, project,
     u'AdaCore', doc_name, doc_name, '')]


def setup(app):
    app.add_lexer('ada', ada_pygments.AdaLexer)
    app.add_lexer('gpr', ada_pygments.GNATProjectLexer)
