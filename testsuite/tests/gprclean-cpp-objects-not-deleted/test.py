import os
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN, GPRLS


bnr = BuilderAndRunner()


def check_paths_deleted(paths):
    for path in paths:
        if os.path.exists(path):
            print(path + " not deleted")


def run(args):
    print(bnr.check_output(args).out)


# build/clean 'extending' project
run(["gprbuild", "-p", "-q", "-Pextending"])
run([GPRLS, "-Pextending", "-s", "-U"])
run([GPRCLEAN, "-p", "-Pextending"])
check_paths_deleted(["obj"])
