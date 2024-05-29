from os import makedirs
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN

bnr = BuilderAndRunner()

makedirs('test/static')
bnr.call([GPRCLEAN, "-r", "prj.gpr", "-q"])
