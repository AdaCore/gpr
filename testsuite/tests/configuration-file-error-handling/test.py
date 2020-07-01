from e3.os.process import Run

import sys

print(Run('gpr2clean --config=no.cgpr -P p.gpr'.split(), output=None).status)
sys.stdout.flush()

print(Run('gpr2install --prefix=p --config=no.cgpr -P p.gpr'.split(),
          output=None).status)
