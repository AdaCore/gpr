import os
import subprocess

from e3.os.process import Run
from e3.env import Env
from e3.fs import cp
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRBUILD, GPRINSTALL

p = Run([GPRBUILD, '-p', 'inst.gpr'])

p = BuilderAndRunner().run([GPRINSTALL, '-p',
                            '--prefix=' + os.path.join(os.getcwd(), 'inst'),
                            'inst.gpr'])

for P in [ 'inst/include/inst/pack.ads',
           'inst/include/inst/pack-impl.ads',
           'inst/include/inst/pack-impl__unix.adb',
           'inst/lib/inst/pack.ali',
           'inst/lib/inst/pack-impl.ali',
           'inst/lib/inst/pack-impl__unix.ali',
           'inst/lib/inst/libnex.so',
           'inst/lib/inst/libnex.dll' ]:
    if os.path.exists(P):
        print("FOUND    : " + os.path.basename(P))
    else:
        print("NOTFOUND : " + P)

p = Run([GPRBUILD, '-p', '-q', 'shr.gpr'], output="s.out")

for L in sorted(open("s.out").readlines()):
    if not ("NOTE:" in L or "warning:" in L):
        print(L)
