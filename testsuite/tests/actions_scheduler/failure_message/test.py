from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()
test_number = 1


def test(header, scenario_idx, expected_message=""):
    global test_number
    proc = bnr.call(["./bin/test", str(scenario_idx)], quiet=True)
    output = proc.out.strip()

    if expected_message:
        if expected_message in output:
            result = "OK"
        else:
            result = "KO"
    else:
        # No output expected at all
        if output == "":
            result = "OK"
        else:
            result = "KO"

    print("Case " + str(test_number) + " - " + header + ": " + result)
    test_number += 1


bnr.build(project="test.gpr", args=["-p", "-q"])

test("Custom failure message", 1,
     expected_message="custom error: failing action could not complete")
test("Empty failure message", 2)
