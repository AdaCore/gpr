import sys
import os
sys.path.insert(0, os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', 'share')))
import gpr_shared_conf
gpr_shared_conf.configure(globals(), u'GPR Documentation', 2024)
html_use_index = False
