import os
from e3.fs import mkdir, sync_tree
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS

dest = 'dst'

# return longest source file path if 'name' used as intermediate path
def longest_path_length (name):
    return len(os.getcwd() + '/' + dest + '/' + name + '/src1/test1.ads')

dir_name = 'a'

while longest_path_length (dir_name) < 255 :
    dir_name = dir_name + 'a'

src_path = os.path.join ('src_files')
dest_path = os.path.join (dest, dir_name)

mkdir(dest_path)
sync_tree (src_path, dest_path)

prj_gpr = open('prj.gpr', 'w')
prj_gpr.write('with"' + dir_name + '/test1";project Prj is for Main use("main.adb");end Prj;')
prj_gpr.close()

test1_gpr = open(os.path.join (dest, dir_name, 'test1.gpr'), 'w')
test1_gpr.write('with"' + dir_name + '/test2";project Test1 is for Source_Dirs use("src1");end Test1;')
test1_gpr.close()

p = BuilderAndRunner().run(
    [GPRLS, "-U", "-P", "prj.gpr"],
    env={"GPR_PROJECT_PATH": os.getcwd() + '/' + dest},
    output='gprls.out',
)

for line in open('gprls.out'):
    print(line.replace(dir_name,'dir_name'))

