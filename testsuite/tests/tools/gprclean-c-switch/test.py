from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN, GPRBUILD


bnr = BuilderAndRunner()

print('TEST1')
bnr.check_output([GPRBUILD, '-p', '-q', '-Pfiles/test1', '--subdirs=debug'])
print(bnr.check_output([GPRCLEAN, '-r', '-n', '-c', '-Pfiles/test1',
                        '--subdirs=debug']).out)
print('TEST2')
bnr.check_output([GPRBUILD, '-p', '-q', '-Pfiles/test2'])
print(bnr.check_output([GPRCLEAN, '-r', '-n', '-c', '-Pfiles/test2']).out)
