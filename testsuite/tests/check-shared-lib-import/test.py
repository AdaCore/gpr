from testsuite_support.builder_and_runner import BuilderAndRunner

subprocess = BuilderAndRunner()

subprocess.call(["gprclean", "-r", "prj.gpr"])
subprocess.call(["gprclean", "-r", "--unchecked-shared-lib-imports",
                 "prj.gpr"])

# TODO: reactivate code below when we know the name of switch disabling the
# warnings.
if False:
    subprocess.call(["gprclean", "-ws", "-r",
                     "--unchecked-shared-lib-imports", "prj.gpr"])
