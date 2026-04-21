import os

from e3.env import Env
from e3.fs import cp
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSTALL

bnr = BuilderAndRunner()
bnr.build("prj.gpr")

os.mkdir("lib/gnatprove")
os.mkdir("lib/gnatprove/phase1")
fh = open ("lib/gnatprove/api.ali", "w")
fh = open ("lib/gnatprove/phase1/api.ali", "w")

bnr.run([GPRINSTALL, '-p', '--prefix=' + os.path.join(os.getcwd(), 'inst'),
         'prj.gpr'])

if os.path.exists('inst/lib/prj/gnatprove'):
    print("OK gnatprove")
else:
    print("NOK")

if os.path.exists('inst/lib/prj/gnatprove/api.ali'):
    print("OK gnatprove/api.ali")
else:
    print("NOK")

if os.path.exists('inst/lib/prj/gnatprove/phase1/api.ali'):
    print("OK gnatprove/phase1/api.ali")
else:
    print("NOK")
