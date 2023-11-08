from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSPECT

bnr = BuilderAndRunner()

def filter(file):
    projects_information_found = False
    with open(file) as F:
        for line in F:
            if "    - Attributes        :" in line:
                projects_information_found = True
            if projects_information_found:
                print(line.rstrip())

def filter_output(output):
    projects_information_found = False
    for line in output.split("\n"):
        if "    - Attributes        :" in line:
            projects_information_found = True
        if projects_information_found:
            print(line)

cmd = [GPRINSPECT, '-Ptest', '--packages', '--attributes']
r = bnr.run(cmd, output="gnatinspect.out")
filter("gnatinspect.out")
