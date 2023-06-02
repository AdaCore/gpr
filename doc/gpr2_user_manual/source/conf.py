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

import sys
import os
import time
import re

sys.path.append('.')

DOCS = {
    'gpr2_um': {
        'title': u'GPR2 Library User Manual'}}
doc_name = 'gpr2_um'

# Then retrieve the source directory
root_source_dir = os.path.dirname(os.path.abspath(__file__))
gpr_version_spec = os.path.join(root_source_dir, '..', '..', '..', 'tools', 'src',
                                'gpr2-version.ads')

with open(gpr_version_spec, 'r') as fd:
    gpr_version_content = fd.read()

def get_copyright():
    return u'2022-%s, AdaCore' % time.strftime('%Y')

def get_gpr_version():
    m = re.search(r'Short_Value : ' +
                  r'constant String := "([^"]+)";',
                  gpr_version_content)
    if m:
        return m.group(1).strip()
    print('cannot find GPR version in ' + gpr_version_spec)
    return 'unknown'

# -- Project information -----------------------------------------------------

project = DOCS[doc_name]['title']

copyright = get_copyright()

version = get_gpr_version()

author = 'AdaCore'

# The full version, including alpha/beta/rc tags
release = get_gpr_version()


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    "sphinx.ext.autodoc",
    "sphinx.ext.coverage",
    "sphinx.ext.imgmath",
    "sphinx.ext.ifconfig",
    "sphinx_rtd_theme",
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = []


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']
