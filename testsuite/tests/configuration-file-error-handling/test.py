from e3.os.process import Run

from utils import filter_cwd

p = Run('gpr2clean --config=no.cgpr -P p.gpr'.split())
print(p.status)
print(filter_cwd(p.out))

p = Run('gpr2install --prefix=p --config=no.cgpr -P p.gpr'.split())
print(p.status)
print(filter_cwd(p.out))
