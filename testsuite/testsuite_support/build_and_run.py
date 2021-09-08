import os
import os.path

from e3.testsuite.driver.classic import TestAbortWithError
from e3.env import Env

from testsuite_support.base_driver import BaseDriver, create_fake_ada_compiler
from testsuite_support.builder_and_runner import BuilderAndRunner


class BuildAndRunDriver(BaseDriver):
    """
    Driver to build a test program and run it.

    Interface:

    * put a project file for a program and the associated source files in the
      test directory;
    * put a "test.out" text file in the test directory;
    * in the "test.yaml" file, add a "project_file" key that contains the name
      of the project file, and add also a "main" key that contains the name of
      the program to run.

    This driver will build the program using GPRbuild and will then run the
    program. The output of this run is checked against the expected output
    (test.out file).
    """

    def set_up(self):
        super(BuildAndRunDriver, self).set_up()

        project_file = self.test_env.get("project_file", None)
        main = self.test_env.get("main", None)

        if not project_file or not isinstance(project_file, str):
            raise TestAbortWithError(
                'test.yaml: please define a "project_file" string field'
            )
        if not main or not isinstance(main, str):
            raise TestAbortWithError('test.yaml: please define a "main" string field')

        self.project_file = project_file
        self.main_program = main
        self.fake_ada_target = self.test_env.get("fake_ada_target", None)

        self.builder_and_runner = BuilderAndRunner(self)

    def run(self):
        env = {}

        # Build the program and run it
        # Note: do it *before* creating fake toolchains, in case a native
        # fake compiler is requested.
        self.builder_and_runner.build(
            project=self.project_file,
            args=["-g1", "-q", "-p", "-bargs", "-Es"],
            env=env,
        )

        # If we are requested to run with a fake toolchain, set it up now
        if self.fake_ada_target:
            if isinstance(self.fake_ada_target, list):
                targets = self.fake_ada_target
            else:
                targets = [self.fake_ada_target]
            paths = []
            for tgt in targets:
                fake_dir = self.working_dir("fake-ada-%s" % tgt)
                paths.append(os.path.join(fake_dir, "bin"))

                if 'linux' in tgt or 'windows' in tgt:
                    is_cross = False
                elif tgt == 'native':
                    is_cross = False
                    tgt = Env().host.triplet
                else:
                    is_cross = True
                if not is_cross:
                    rts = ["native", "sjlj", "light"]
                elif 'vxworks' in tgt:
                    rts = ["rtp"]
                else:
                    assert False, "unexpected fake target %s" % tgt

                create_fake_ada_compiler(
                    self,
                    comp_dir=fake_dir,
                    comp_target=tgt,
                    gnat_version="21.0w",
                    gcc_version="8.4.3",
                    runtimes=rts,
                    comp_is_cross=is_cross,
                )
            paths.append(os.environ.get("PATH"))
            env["PATH"] = os.pathsep.join(paths)

        self.builder_and_runner.run([os.path.join(".", self.main_program)], env=env)
