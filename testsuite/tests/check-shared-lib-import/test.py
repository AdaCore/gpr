from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

subprocess = BuilderAndRunner()

subprocess.call([GPR2BUILD, "-p", "-c", "-q", "prj.gpr"])
subprocess.call([GPR2BUILD, "-p", "-c", "-q",
                 "--unchecked-shared-lib-imports", "prj.gpr"])
