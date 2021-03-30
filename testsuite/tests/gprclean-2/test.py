import os
from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN


def check_paths_deleted(paths):
    for path in paths:
        if os.path.exists(path):
            print(path + ' not deleted')


bnr = BuilderAndRunner()

if Env().host.platform.endswith('windows'):
    main_adb = 'Main.Adb'
    test = 'Test'
else:
    main_adb = 'main.adb'
    test = 'test'

# build/clean 'files/test' project
bnr.check_output(['gprbuild', '-p', '-q', '-Pfiles/test'])
bnr.check_output([GPRCLEAN, '-r', '-p', '-q', '-Pfiles/test', main_adb])
check_paths_deleted(['obj', 'obj1'])

# build/clean 'files/test' project
bnr.check_output(['gprbuild', '-p', '-q', '-Pfiles/test'])
bnr.check_output([GPRCLEAN, '-r', '-p', '-q', '-Pfiles/test', test, 'test1'])
check_paths_deleted(['obj1/test1.o'])
check_paths_deleted(['obj/test.o'])
bnr.check_output([GPRCLEAN, '-r', '-p', '-q', '-Pfiles/test', 'main'])
check_paths_deleted(['obj', 'obj1'])
