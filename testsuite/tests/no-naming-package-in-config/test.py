from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN

p = BuilderAndRunner().run([GPRCLEAN, '-p', '-q', 'p.gpr', '--config=p.cgpr'])
print(p.out)
