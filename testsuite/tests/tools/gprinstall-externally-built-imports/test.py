import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSTALL

BuilderAndRunner().run([GPRINSTALL, '-p',
                        '--prefix=' + os.path.join(os.getcwd(), 'inst'),
                        'prj.gpr'])

if os.path.exists('inst/share/gpr/prj.gpr'):
	with open('inst/share/gpr/prj.gpr', "r+") as fp:
		for line in fp:
			if "with" in line:
				print(line.split(";")[0] + ";")
else:
    print("NOK")
