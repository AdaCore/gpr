from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSPECT

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRINSPECT):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

#run([GPRBUILD, "-f", "-u", "-P", "prj.gpr"])

#=========================
# Previous test.sh content
#=========================
#gnatmake -q filter
#gprbuild -f -vP2 -p -PCODE/prj > tmpout.txt 2>&1
#./filter < this was printing the line where the exec dir was displayed, parse the gprinspect output instead