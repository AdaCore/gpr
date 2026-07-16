import os
import sys

from e3.os.process import Run
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

bnr = BuilderAndRunner()

p = Run([GPRBUILD, '-p', 'mylib/mylib.gpr'])

p = bnr.run([GPRINSTALL, '-p',
             '--prefix=' + os.path.join(os.getcwd(), 'inst'),
             'mylib/mylib.gpr'])

if os.path.exists('inst/lib/mylib/counters.sid'):
    print("OK counters.sid")
else:
    print("NOK counters.sid")

if os.path.exists('inst/lib/mylib/ccode.c.sid'):
    print("OK ccode.c.sid")
else:
    print("NOK ccode.c.sid")

if os.path.exists('inst/lib/mylib/cppcode.cpp.sid'):
    print("OK cppcode.cpp.sid")
else:
    print("NOK cppcode.cpp.sid")
