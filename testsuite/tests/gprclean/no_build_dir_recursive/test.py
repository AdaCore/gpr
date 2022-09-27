from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN
import re
import os

bnr = BuilderAndRunner()

# Build the imported project (so that it has something to clean), but not
# the toplevel project (which does not even have its object directory)
bnr.run(["gprbuild", "-p", "-q", "-Pprj2", "-u"])

# Running gprclean recursively should still clean the imported project)
# gprclean should not complain (ie warning, not error) that the object
# directory is missing for the toplevel project)

output = "tmp.out"

status = bnr.run([GPRCLEAN, "-r", "-Pdefault"],
                 output=output).status
f = open(output)
for line in f:
    print(re.sub("default.gpr:.:[0-9]+: ", "", line))
f.close()

# There should be nothing left in the object directory of the imported
# project
os.system('ls obj2')
