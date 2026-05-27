from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

# Build the executable
bnr.build("prj.gpr")

# Tests

# This should be silent
print("TEST 1")
bnr.call(["./main", "-Pprj"])

# This should print "using project file /prj.gpr"
print("TEST 2")
r = bnr.run(["./main", "--load-project"])
assert "using project file" in r.out

# This test should flag a non existing switch
print("TEST 3")
r = bnr.run(["./main", "-Pprj", "--load-project", "--nonexistentswitch"])
assert "Unrecognized argument \"--nonexistentswitch\"" in r.out
# ... and print the help: we check that the help is printed
# by checking this...
assert "usage: main" in r.out
# ... and these bits, part of the help for project switches
assert "Print the GPR registry" in r.out
assert "--relocate-build-tree" in r.out

# This test tries to use almost all the switches
print("TEST 4")
r = bnr.run(
    [
        "./main",
        "-Ptest_data/p",
        "--load-project",
        "-aP",
        "..",
        "--autoconf",
        "truc.conf",
        "--config",
        "test_data/p.gpr",
        "--db",
        "src",
        "--implicit-with",
        "gpr2",
        "-eL",
        "--print-gpr-registry",
        "--root-dir",
        "root",
        "--relocate-build-tree",
        "--RTS=truc",
        "--RTS:c=truc",
        "--RTS:c++=bla",
        "--subdirs",
        "src",
        "--src-subdirs",
        "foo",
        "-Xa=b",
        "-X",
        "C=D",
    ]
)
print(r.out)  # The output should be empty

# This tests the exception mechanism
print("TEST 5")
r = bnr.run(
    [
        "./main",
        "-Pprj",
        "--no-project",
        "--db-",
        "--target",
        "unknown",
        "--load-project"
    ]
)
assert "cannot specify --no-project with a project file" in r.out
