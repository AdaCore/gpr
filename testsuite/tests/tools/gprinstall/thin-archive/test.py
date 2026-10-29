from pathlib import Path

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

bnr = BuilderAndRunner()


def check_program():
    executable = Path("consumer/obj/main").resolve()
    result = bnr.run([str(executable)], catch_error=True)
    assert result.out.strip() == "42", result.out


for mode in ("thin", "normal"):
    prefix = Path("installed-" + mode).resolve()
    bnr.run([GPRBUILD, "-Plibrary/lib.gpr", "-p", "-q",
             "-XARCHIVE_MODE=" + mode], catch_error=True)
    bnr.run([GPRINSTALL, "-Plibrary/lib.gpr", "-p", "-q",
             "--prefix=" + str(prefix), "-XARCHIVE_MODE=" + mode],
            catch_error=True)

    # The consumer imports the installed project, which is externally built.
    command = [GPRBUILD, "-Pconsumer/app.gpr", "-p", "-q", "-f",
               "-aP" + str(prefix / "share/gpr")]
    bnr.run(command, catch_error=True)
    check_program()

    # Installing a thin archive leaves it dependent on the original objects.
    Path("library/obj").rename("library/saved-obj")
    result = bnr.run(command, catch_error=False)
    if mode == "thin":
        assert result.status != 0 and "value.o" in result.out, result.out
        print("installed thin archive still needs the build objects: OK")
    else:
        assert result.status == 0, result.out
        check_program()
        print("installed normal archive works without the build objects: OK")
    Path("library/saved-obj").rename("library/obj")
