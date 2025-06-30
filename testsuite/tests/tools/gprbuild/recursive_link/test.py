from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

bnr = BuilderAndRunner()



def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == GPR2BUILD:
        out = bnr.run(cmd).out
        # library dependency cycles are not displayed consistently, we need
        # to order the output
        before=[]
        deps=[]
        rem=[]
        for l in out.splitlines():
            if "depends on" in l:
                deps.append(l)
            elif len(deps) > 0:
                rem.append(l)
            else:
                before.append(l)
        print('\n'.join(before + sorted(deps) + rem))
    else:
        print(bnr.simple_run(cmd).out)

run([GPR2BUILD, "-q", "prj.gpr", "-j1"])
run(["./main"])
