import os, stat
from gnatpython.ex import Run

# build 'p' project
Run(['gprbuild', '-p', '-q', '-Pp'])

# set main.ali read only
os.chmod('main.ali', stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH)

# clean project in normal mode
Run(['gpr2clean', '-Pp'])

# check read only main.ali not deleted
if not os.path.exists('main.ali'):
    print ('NOK read-only files deleted in normal mode')

else:
    # clean project forcing deletions
    Run(['gpr2clean', '-f', '-Pp'])

    # check read only main.ali deleted
    if os.path.exists('main.ali'):
        print ('NOK read-only files not deleted in force deletions mode')
    else:
        print ('OK')
