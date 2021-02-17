from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN, GPRINSTALL

import sys

bnr = BuilderAndRunner()
print(bnr.run([GPRCLEAN, "--config=no.cgpr", "-P", "p.gpr"],
              output=None).status)
sys.stdout.flush()

print(bnr.run([GPRINSTALL, "--prefix=p", "--config=no.cgpr", "-P", "p.gpr"],
              output=None).status)
