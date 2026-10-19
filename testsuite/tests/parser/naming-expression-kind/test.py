from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD


bnr = BuilderAndRunner()

for project, names, args in (
    ("test.gpr", ("Body",), []),
    (
        "other_lists.gpr",
        ("Implementation", "Spec", "Specification"),
        ["-XUSE_LISTS=true"],
    ),
    (
        "other_lists.gpr",
        ("Implementation", "Spec", "Specification"),
        ["-XUSE_LISTS=false"],
    ),
):
    result = bnr.run([GPRBUILD, "-P", project, "-p", "-q"] + args)
    assert result.status != 0, result.out
    assert "ASSERTION_ERROR" not in result.out, result.out
    print(" ".join([project] + args))
    for name in names:
        diagnostic = 'attribute "Naming\'' + name + '" expects a single value'
        assert diagnostic in result.out, result.out
        print(diagnostic)
