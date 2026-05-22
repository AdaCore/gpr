"""Shared Sphinx configuration for all GPR documentation subdocs."""
import sys
import os
import shutil
import time
import re


def configure(g, doc_title, copyright_start_year=2024):
    """Populate Sphinx globals for a GPR documentation subdoc.

    Call as the last statement in a subdoc's conf.py::

        gpr_shared_conf.configure(globals(), 'My Title', 2024)
    """
    source_dir = os.path.dirname(os.path.abspath(g['__file__']))
    share_dir = os.path.normpath(os.path.join(source_dir, '..', '..', 'share'))
    sys.path.insert(0, share_dir)

    gpr_version_spec = os.path.normpath(os.path.join(
        source_dir, '..', '..', '..', 'src', 'lib', 'gpr2-version.ads'))
    with open(gpr_version_spec) as f:
        content = f.read()

    m = re.search(r'Short_Value : constant String := "([^"]+)";', content)
    ver = m.group(1).strip() if m else 'unknown'

    g.update(dict(
        project=doc_title,
        author='AdaCore',
        copyright='%d-%s, AdaCore' % (copyright_start_year, time.strftime('%Y')),
        version=ver,
        release=ver,
        extensions=[
            'sphinx.ext.autodoc',
            'sphinx.ext.coverage',
            'sphinx.ext.imgmath',
            'sphinx.ext.ifconfig',
            'sphinx_rtd_theme',
        ],
        templates_path=[os.path.join(share_dir, '_templates')],
        html_css_files=[],
        exclude_patterns=[],
        html_theme='sphinx_rtd_theme',
        html_theme_options={'style_nav_header_background': '#12284c'},
        html_static_path=[os.path.join(share_dir, '_static')],
        html_logo=os.path.join(share_dir, 'adacore-logo-white.png'),
        html_favicon=os.path.join(share_dir, 'favicon.ico'),
    ))

    def setup(app):
        import ada_pygments
        app.add_lexer('ada', ada_pygments.AdaLexer)
        app.add_lexer('gpr', ada_pygments.GNATProjectLexer)

    g['setup'] = setup

    subdoc_name = os.path.basename(os.path.dirname(source_dir))
    g['latex_documents'] = [
        ('index', subdoc_name + '.tex', doc_title, 'AdaCore', 'manual'),
    ]

    # If pagefind is available, enable improved search indexing.
    if shutil.which('pagefind') is not None:
        g['extensions'].append('pagefind-sphinx')
        g['templates_path'].append(
            os.path.join(share_dir, '_templates_pagefind'))
        g['html_css_files'].append('pagefind.css')
