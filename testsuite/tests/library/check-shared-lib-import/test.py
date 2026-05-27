from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

subprocess = BuilderAndRunner()

subprocess.call([GPRBUILD, "-p", "-c", "-q", "prj.gpr"])
subprocess.call([GPRBUILD, "-p", "-c", "-q",
                 "--unchecked-shared-lib-imports", "prj.gpr"])
