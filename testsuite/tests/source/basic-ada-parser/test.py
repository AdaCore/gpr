from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
import os

bnr = BuilderAndRunner()

bnr.build('test.gpr', args=['-p'])

files_to_parse_dir = "files-to-parse"
files_to_parse = os.listdir(files_to_parse_dir)

# os.listdir lists in an arbitrary order. Order
# is important here, because we compare the output with
# the expected output file test.out.
files_to_parse.sort()

first = True
for file in files_to_parse:
    bnr.run(['./main', os.path.join(files_to_parse_dir, file)], output="run.out")

    if first:
        first = False
    else:
        print("")

    print("===== FILE " + file + "=====")
    for line in open("run.out"):
        li = line[:-1]
        print(li)
