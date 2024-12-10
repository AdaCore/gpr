from testsuite_support.builder_and_runner import BuilderAndRunner

subprocess = BuilderAndRunner()

subprocess.call(["gpr2build", "-p", "-c", "-q", "prj.gpr"])
subprocess.call(["gpr2build", "-p", "-c", "-q",
                 "--unchecked-shared-lib-imports", "prj.gpr"])
