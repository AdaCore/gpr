from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2CLEAN, GPR2BUILD


bnr = BuilderAndRunner()

print('TEST1')
bnr.check_output([GPR2BUILD, '-p', '-q', '-Pfiles/test1', '--subdirs=debug'])
print(bnr.check_output([GPR2CLEAN, '-r', '-n', '-c', '-Pfiles/test1',
                        '--subdirs=debug']).out)
print('TEST2')
bnr.check_output([GPR2BUILD, '-p', '-q', '-Pfiles/test2'])
print(bnr.check_output([GPR2CLEAN, '-r', '-n', '-c', '-Pfiles/test2']).out)
