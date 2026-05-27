from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRCLEAN

p = BuilderAndRunner().run([GPRCLEAN, '-p', '-q', 'p.gpr', '--config=p.cgpr'])
print(p.out)
