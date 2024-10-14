import os
import sys

from e3.os.process import Run
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL, GPRBUILD

bnr = BuilderAndRunner()

p = Run([GPRBUILD, '-p', 'mylib/mylib.gpr'])

p = bnr.run([GPRINSTALL, '-p',
             '--prefix=' + os.path.join(os.getcwd(), 'inst'),
             'mylib/mylib.gpr'])

p = Run([GPRBUILD, '-p', '-f', '--src-subdirs=instr', 'mylib/mylib.gpr'])

p = bnr.run([GPRINSTALL, '-p',
             '--prefix=' + os.path.join(os.getcwd(), 'inst'),
             '--sources-subdir=include/mylib/instr',
             '--src-subdirs=instr',
             '--build-name=instr',
             'mylib/mylib.gpr'])

if os.path.exists('inst/include/mylib/counters.adb'):
    print("OK counters.adb")
else:
    print("NOK counters.adb")

if os.path.exists('inst/include/mylib/instr/counters.adb'):
    print("OK instrumented counters.adb")
else:
    print("NOK instrumented counters.adb")

if os.path.exists('inst/lib/mylib/counters.sid'):
    print("OK counters.sid")
else:
    print("NOK counters.sid")

if os.path.exists('inst/lib/mylib.instr/counters.sid'):
    print("OK instrumented counters.sid")
else:
    print("NOK instrumented counters.sid")

sys.stdout.flush()

# And now tests building two test driver

p = Run([GPRBUILD, '-p', 'libtests/libtests.gpr'])
p = Run(['libtests/obj/main'], output=None)

p = Run([GPRBUILD, '-p', '-f',
         '-XMYLIB_BUILD=instr', 'libtests/libtests.gpr'])
p = Run(['libtests/obj/main'], output=None)
