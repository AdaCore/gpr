from e3.os.process import Run

p = Run('gpr2clean -p -q p.gpr --config=p.cgpr'.split())
print(p.out)
