from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN

subprocess = BuilderAndRunner()

subprocess.call([GPRCLEAN, "-r", "prj.gpr"])
subprocess.call([GPRCLEAN, "-r", "--unchecked-shared-lib-imports",
                 "prj.gpr"])

# TODO: reactivate code below when we know the name of switch disabling the
# warnings.
if False:
    subprocess.call([GPRCLEAN, "-ws", "-r",
                     "--unchecked-shared-lib-imports", "prj.gpr"])
