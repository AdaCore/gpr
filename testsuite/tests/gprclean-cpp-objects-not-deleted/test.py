import os
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN


bnr = BuilderAndRunner()


def check_paths_deleted(paths):
    for path in paths:
        if os.path.exists(path):
            print(path + " not deleted")


def run(args):
    bnr.check_output(args)


# clean 'extending' project & check obj dir deleted
run([GPRCLEAN, "-p", "-Pextending"])
check_paths_deleted(["obj"])
