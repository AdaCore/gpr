from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS, GPRBUILD

bnr = BuilderAndRunner()

bnr.run([GPRBUILD, "-p", "-q", "-Pprj.gpr"])

output = "output.txt"

status = bnr.run([GPRLS, "-P", "prj.gpr", "--closure"],
                 output=output).status

for line in open("output.txt").readlines():
    if "pkg-execute.adb" in line:
        print(line)

for line in open("output.txt").readlines():
    if "pkg-sub.adb" in line:
        print(line)
