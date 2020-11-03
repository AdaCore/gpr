from testsuite_support.builder_and_runner import BuilderAndRunner

p = BuilderAndRunner().run(['gpr2clean', '-p', '-q', 'p.gpr',
                            '--config=p.cgpr'])
print(p.out)
