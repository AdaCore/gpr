from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD


bnr = BuilderAndRunner()

for project, names, expected in (
    (
        "single_expected.gpr",
        (
            "Object_Dir",
            "Target",
            "Compiler'Local_Configuration_Pragmas",
            "Builder'Global_Configuration_Pragmas",
        ),
        "a single value",
    ),
    (
        "list_expected.gpr",
        (
            "Source_Dirs",
            "Source_Files",
            "Compiler'Switches",
            "Naming'Implementation_Exceptions",
        ),
        "a list of values",
    ),
    ("registry_dirs.gpr", ("Gpr_Registry_Dirs",), "a list of values"),
):
    result = bnr.run([GPRBUILD, "-P", project, "-p", "-q"])
    assert result.status != 0, result.out
    assert "ASSERTION_ERROR" not in result.out, result.out
    print(project)
    for name in names:
        diagnostic = f'attribute "{name}" expects {expected}'
        assert diagnostic in result.out, result.out
        print(diagnostic)

bnr.check_output([GPRBUILD, "-P", "valid.gpr", "-p", "-q"])
print("Valid scalar, list, and unregistered attributes: OK")
