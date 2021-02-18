import os
import re
from e3.testsuite.driver.classic import TestAbortWithError
from e3.testsuite.driver.classic import ClassicTestDriver
from e3.fs import cp
from testsuite_support.base_driver import create_fake_ada_compiler
from testsuite_support.builder_and_runner import BuilderAndRunner

TESTSUITE_ROOT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ADA_SUPPORT_FILES = os.path.join(TESTSUITE_ROOT_DIR, "support")


class BasicDriver(ClassicTestDriver):
    """
    Driver to build a test program and run it.

    Interface:

    * put a main in test.adb that will be compiled (using a default project)
    * use Test_Assert functions to make your assertions
    * return Test_Assert.Report at the end of our main procedure

    This driver will build the program using GPRbuild and will then run the
    program and ensure that all assertions did passed and that the program
    did not crashed
    """

    def set_up(self):
        self.project_file = self.test_env.get("project_file")
        if self.project_file is None:
            cp(
                os.path.join(ADA_SUPPORT_FILES, "test.gpr"),
                self.test_env["working_dir"],
            )
            self.project_file = "./test.gpr"
            self.main = "./obj/test"
        else:
            self.main = self.test_env.get("./main")

        if not self.project_file or not isinstance(self.project_file, str):
            raise TestAbortWithError(
                'test.yaml: please define a "project_file" string field'
            )
        if not self.main or not isinstance(self.main, str):
            raise TestAbortWithError('test.yaml: please define a "main" string field')

        self.fake_ada_target = self.test_env.get("fake_ada_target")
        self.builder_and_runner = BuilderAndRunner(self)

    def run(self):
        env = {"ADA_SUPPORT_SOURCE_DIR": ADA_SUPPORT_FILES}

        # If we are requested to run with a fake toolchain, set it up now
        if self.fake_ada_target:
            fake_dir = self.working_dir("fake-ada")
            create_fake_ada_compiler(
                self,
                comp_dir=fake_dir,
                comp_target=self.fake_ada_target,
                gnat_version="21.0w",
                gcc_version="8.4.3",
                runtimes=["rtp"],
                comp_is_cross=True,
            )
            env["PATH"] = os.path.join(fake_dir, "bin") + os.path.pathsep + env["PATH"]

        # Build the program and run it
        p = self.builder_and_runner.build(
            project=self.project_file,
            args=["-g1", "-p", "-bargs", "-Es"],
            env=env,
        )
        self.output += p.out
        p = self.builder_and_runner.run([self.main], env=env)
        self.output += p.out
        if "Call stack traceback locations" in p.out:
            m = re.search(r"Call stack traceback locations:.*\n([^\r\n]+)", p.out)
            if m.group(1) is not None:
                self.shell(["addr2line", "-e", self.main] + m.group(1).split())

    def compute_failures(self):
        return (
            ["Success marker not found"]
            if "<=== TEST PASSED ===>" not in self.result.log.log
            else []
        )
