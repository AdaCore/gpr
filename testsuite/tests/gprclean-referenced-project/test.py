import os
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN


def check_paths_deleted(paths):
    for path in paths:
        if os.path.exists(path):
            print(path + ' not deleted')


def check_paths_not_deleted(paths):
    for path in paths:
        if not os.path.exists(path):
            print(path + ' deleted')


bnr = BuilderAndRunner()

print("*** Not recursive GPRClean on extended project ***")

# Check if extended project is not cleaned
bnr.check_output(['gprbuild', '-p', '-q', '-Pprj2.gpr'])
bnr.check_output(['gprbuild', '-p', '-q', '-Pprj1.gpr'])
bnr.check_output([GPRCLEAN, '-p', '-q', '-Pprj2.gpr'])
check_paths_deleted(['obj2'])
check_paths_not_deleted(['obj1'])


print("*** Recursive GPRClean on extended project ***")

# Check if extended project is cleaned in recurse mode
bnr.check_output(['gprbuild', '-p', '-q', '-Pprj2.gpr'])
bnr.check_output([GPRCLEAN, '-p', '-q', '-Pprj2.gpr', '-r'])
check_paths_deleted(['obj1', 'obj2'])