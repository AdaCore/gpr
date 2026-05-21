import sys
import os
import time
import re

source_dir = os.path.dirname(os.path.abspath(__file__))
share_dir = os.path.normpath(os.path.join(source_dir, '..', '..', 'share'))
sys.path.insert(0, share_dir)

gpr_version_spec = os.path.normpath(os.path.join(
    source_dir, '..', '..', '..', 'src', 'lib', 'gpr2-version.ads'))
with open(gpr_version_spec) as f:
    content = f.read()

m = re.search(r'Short_Value : constant String := "([^"]+)";', content)
ver = m.group(1).strip() if m else 'unknown'

project = 'GPR Documentation'
author = 'AdaCore'
copyright = '2024-%s, AdaCore' % time.strftime('%Y')
version = ver
release = ver

extensions = ['sphinx_rtd_theme']
templates_path = [os.path.join(source_dir, '_templates')]
root_doc = 'gprbuild_ug'

html_theme = 'sphinx_rtd_theme'
html_theme_options = {'style_nav_header_background': '#12284c'}
html_static_path = [os.path.join(share_dir, '_static')]
html_logo = os.path.join(share_dir, 'adacore-logo-white.png')
html_favicon = os.path.join(share_dir, 'favicon.ico')
html_use_index = False


def setup(app):
    import ada_pygments
    app.add_lexer('ada', ada_pygments.AdaLexer)
    app.add_lexer('gpr', ada_pygments.GNATProjectLexer)
