import os

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL

BuilderAndRunner().run([GPRINSTALL, '-p',
                        '--prefix=' + os.path.join(os.getcwd(), 'inst'),
                        'agg.gpr'])

if os.path.exists('inst/share/gpr/agg.gpr'):
	with open('inst/share/gpr/agg.gpr', "r+") as fp:
		for line in fp:
			if "with" in line:
				print(line.split(";")[0] + ";")
else:
    print("NOK")
