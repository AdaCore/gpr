import os
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN


def check_paths_deleted(paths):
    for path in paths:
        if os.path.exists(path):
            print(path + ' not deleted')


bnr = BuilderAndRunner()

# build/clean 'files/test' project
bnr.check_output(['gprbuild', '-p', '-q', '-Pfiles/test'])
bnr.check_output([GPRCLEAN, '-r', '-p', '-q', '-Pfiles/test', 'main.adb'])
check_paths_deleted(['obj', 'obj1'])
