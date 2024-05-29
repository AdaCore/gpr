import os

from e3.os.process import Run
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL

bnr = BuilderAndRunner()

os.mkdir('obj')
os.mkdir('obj/foo')

p = Run(['gprbuild', '-Pp', '--src-subdirs=foo', '-q'])

p = bnr.run([GPRINSTALL, '-Pp',
             '--prefix=' + os.path.join(os.getcwd(), 'inst'),
             '--src-subdirs=foo', '-p'])

if os.path.exists('inst/include/p/pkg.ads'):
    print("OK pkg.ads")
else:
    print("NOK pkg.ads")

if os.path.exists('inst/lib/p/pkg.ali'):
    print("OK pkg.ali")
else:
    print("NOK pkg.ali")

if os.path.exists('inst/lib/p/pkg.o'):
    print("OK pkg.o")
else:
    print("NOK pkg.o")

if os.path.exists('inst/share/gpr/p.gpr'):
    print("OK p.gpr")
else:
    print("NOK p.gpr")

print(p.out)
