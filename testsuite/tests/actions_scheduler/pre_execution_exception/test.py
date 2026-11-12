from testsuite_support.builder_and_runner import BuilderAndRunner


bnr = BuilderAndRunner()
bnr.build("test.gpr", args=["-p", "-q"])

for kind, diagnostic in [
    ("Scheduler_Error", "Fatal error: scheduler exception from Pre_Execution"),
    ("Unexpected_Exception", "Unexpected exception:"),
]:
    for stop_on_fail in (True, False):
        result = bnr.run(["./test", kind, str(stop_on_fail)])
        assert result.status == 0, result.out
        assert diagnostic in result.out, result.out
        assert "status: FAILED" in result.out, result.out
        assert "action must not execute" not in result.out, result.out
        if kind == "Unexpected_Exception":
            assert "unexpected exception from Pre_Execution" in result.out
        print(f"{kind}, Stop_On_Fail={stop_on_fail}: FAILED")
