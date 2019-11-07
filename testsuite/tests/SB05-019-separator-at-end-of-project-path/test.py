import os
from gnatpython.ex import Run

# set environment with project paths terminated by path separator ':' or ';'
os.environ['GPR_PROJECT_PATH'] =  os.getenv('GPR_PROJECT_PATH', '') + os.pathsep
os.environ['ADA_PROJECT_PATH'] =  os.getenv('ADA_PROJECT_PATH', '') + os.pathsep

# check that gpr2-projects package is able to handle such environment
# without raising an exception
p = Run(['gpr2clean', '-Pp'])
if p.status != 0:
    print ('gpr2clean returned ' + str (p.status))
    print (p.err)
    print (p.out)
else:
    print ('OK')
