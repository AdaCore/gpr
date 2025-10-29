import os
import subprocess

from e3.os.process import Run
from e3.env import Env
from e3.fs import cp
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

p = Run([GPRBUILD, '-p', 'inst.gpr'])

p = BuilderAndRunner().run([GPRINSTALL, '-p',
                            '--prefix=' + os.path.join(os.getcwd(), 'inst'),
                            'inst.gpr'])

for P in [ 'inst/lib/inst/pack.ali',
           'inst/lib/inst/pack__one.ali',
           'inst/lib/inst/pack__one.o' ]:
    if os.path.exists(P):
        print("OK:  " + os.path.basename(P))
    else:
        print("NOK: " + P)
