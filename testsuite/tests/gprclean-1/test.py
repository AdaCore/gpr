import os
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN


def check_paths_deleted(paths):
    for path in paths:
        if os.path.exists('files/' + path):
            print(path + ' not deleted')


bnr = BuilderAndRunner()

# body_suffix for source in Mains
bnr.check_output(['gprbuild', '-p', '-q', '-Pfiles/test', 'main_a_b.a'])
bnr.check_output([GPRCLEAN, '-p', '-q', '-Pfiles/test', 'main_a_b.a'])
check_paths_deleted(['obj'])

# body_suffix & builder'executable for source in Mains
bnr.check_output(['gprbuild', '-p', '-q', '-Pfiles/test', 'main_b_b.a'])
bnr.check_output([GPRCLEAN, '-p', '-q', '-Pfiles/test', 'main_b_b.a'])
check_paths_deleted(['obj'])

# multi-unit source in Mains
bnr.check_output(['gprbuild', '-p', '-q', '-Pfiles/test', 'main_c_b.a'])
bnr.check_output([GPRCLEAN, '-p', '-q', '-Pfiles/test', 'main_c_b.a'])
check_paths_deleted(['obj'])

# body_suffix for source not in Mains
bnr.check_output(['gprbuild', '-p', '-q', '-Pfiles/test', 'main_d_b.a'])
bnr.check_output([GPRCLEAN, '-p', '-q', '-Pfiles/test', 'main_d_b.a'])
check_paths_deleted(['obj'])

# body_suffix & builder'executable for source not in Mains
bnr.check_output(['gprbuild', '-p', '-q', '-Pfiles/test', 'main_e_b.a'])
bnr.check_output([GPRCLEAN, '-p', '-q', '-Pfiles/test', 'main_e_b.a'])
check_paths_deleted(['obj'])

# multi-unit source not in Mains
bnr.check_output(['gprbuild', '-p', '-q', '-Pfiles/test', 'main_f_b.a'])
bnr.check_output([GPRCLEAN, '-p', '-q', '-Pfiles/test', 'main_f_b.a'])
check_paths_deleted(['obj'])

# build/clean 'files/test' project
bnr.check_output(['gprbuild', '-p', '-q', '-Pfiles/test'])
bnr.check_output([GPRCLEAN, '-r', '-p', '-q', '-Pfiles/test'])
check_paths_deleted(['obj1', 'lib1',
                     'obj2', 'lib2', 'lib2-ali', 'lib2-src',
                     'obj3', 'lib3', 'lib3-ali', 'lib3-src',
                     'obj'])
