from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME


bnr = BuilderAndRunner()
bnr.check_output([GPRNAME, "-P", "test.gpr", "--ignore-predefined-units",
                  "*.ada"])

for line in open("test_naming.gpr"):
    print(line, end='')
