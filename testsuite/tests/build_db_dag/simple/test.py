from testsuite_support.builder_and_runner import BuilderAndRunner, GPRBUILD

bnr = BuilderAndRunner()

# ensure .ali and .o files are there for the trees used to test
bnr.call([GPRBUILD, "-Ptree/p1.gpr", "-p", "-q"])
# base1 is extended by p2: ensure it has ali and object files on its own to
# demonstrate that they're inherited
bnr.call([GPRBUILD, "-Ptree/base1.gpr", "-p", "-q"])
bnr.call([GPRBUILD, "-Ptree/p2.gpr", "-p", "-q"])

# now build and run the test (using bnr.build and bnr.run to have proper
# coverage when requested).
bnr.build(project="test.gpr", args=["-p", "-q"])
for gpr in 'tree/agg.gpr', 'clashing_objects/agg.gpr':
    print('-' * (len(gpr) + 6))
    print(f"-- {gpr} --")
    print('-' * (len(gpr) + 6))
    bnr.call(['./main', gpr])
    print("")
