from testsuite_support.builder_and_runner import BuilderAndRunner

import sys

bnr = BuilderAndRunner()
print(bnr.run('gpr2clean --config=no.cgpr -P p.gpr'.split(),
              output=None).status)
sys.stdout.flush()

print(bnr.run('gpr2install --prefix=p --config=no.cgpr -P p.gpr'.split(),
              output=None).status)
