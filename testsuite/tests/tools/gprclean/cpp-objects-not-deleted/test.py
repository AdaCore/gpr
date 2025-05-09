import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD, GPR2CLEAN


bnr = BuilderAndRunner()


def check_paths_deleted(paths):
    for path in paths:
        if os.path.exists(path):
            print(path + " not deleted")


def run(args):
    bnr.check_output(args)


# build then clean 'extending' project & check obj dir deleted
run([GPR2BUILD, "-p", "-Pextending"])
run([GPR2CLEAN, "-p", "-Pextending"])
check_paths_deleted(["obj"])
