#!/usr/bin/env python
import os
from e3.fs import cp
from testsuite_support.builder_and_runner import BuilderAndRunner


def filter(file):
    with open(file) as F:
        for line in F:
            if "Executable_Suffix ->" not in line:
                print(line.rstrip())


bnr = BuilderAndRunner()

# copy GPR2 examples here
found = False
for examples_dir in '../examples', 'examples':
    fulldir = os.path.join(os.environ["root_dir"], examples_dir)
    if os.path.exists(fulldir):
        cp(os.path.join(fulldir, "*"), ".", recursive=True)
        found = True
        break
assert found, "cannot find the examples directory"

os.chdir("casestmt")
bnr.build("casestmt", args=["-p"])
bnr.run(["./main"], output=None)

os.chdir("../simple")
bnr.build("simple", args=["-p"])
bnr.run(["./main"], output=None)

os.chdir("../packages")
bnr.build("packages", args=["-p"])
bnr.run(["./main"], output="packages.out")
filter("packages.out")

os.chdir("../gnatcoll-projects-conversion")
bnr.build("conversion_tutorial", args=["-p"])
bnr.run(["./conversion_tutorial", "-r", "-Pconversion_tutorial"], output=None)
