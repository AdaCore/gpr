import os
import os.path

from e3.fs import mkdir

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCONFIG
from testsuite_support.base_driver import create_fake_ada_runtime


def filter(file):
    with open(file) as F:
        for line in F:
            print(line.rstrip())

runtimes_root = os.path.join(os.getcwd(), "tree")

create_fake_ada_runtime(os.path.join(runtimes_root, "custom-rts"))
mkdir(os.path.join(runtimes_root, "empty-rts"))

env = {}
env["GPR_RUNTIME_PATH"] = runtimes_root

BuilderAndRunner().run(
    [GPRCONFIG, "--batch", "--config=ada,,custom-rts,,"], env=env, output="out1.out"
)
filter("out1.out")

BuilderAndRunner().run(
    [GPRCONFIG, "--batch", "--config=ada,,empty-rts,,"], env=env, output="out2.out"
)
filter("out2.out")

BuilderAndRunner().run(
    [GPRCONFIG, "--batch", "--config=ada,,missing-rts,,"], env=env, output="out3.out"
)
filter("out3.out")
