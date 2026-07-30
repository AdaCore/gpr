from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# Regression test for eng/gpr/gpr-issues#880.
#
# "gprbuild -u <spec>" must compile exactly the source named on the command
# line -- the spec -- even when the unit also has a body. Previously the unit's
# main part (the body) was compiled instead.
#
# Pkg requires a body (it declares a subprogram), so handing its *spec* to the
# compiler is rejected with "cannot generate code for file pkg.ads (package
# spec)". That the error names pkg.ads proves the spec was compiled; had the
# body pkg.adb been compiled instead, the build would have succeeded silently.

out = bnr.simple_run(
    [GPRBUILD, "-P", "prj.gpr", "-p", "-j1", "-u", "pkg.ads"],
    catch_error=False).out

if "cannot generate code" in out and "pkg.ads" in out:
    print("PASS: -u compiled the spec pkg.ads")
else:
    print("FAIL: the spec pkg.ads was not the source handed to the compiler")
    print(out)
