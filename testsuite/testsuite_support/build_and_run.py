import os
import os.path

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, create_fake_ada_compiler, SetupError
)
from gnatpython.env import Env


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

    TIMEOUT = 300

    #
    # Driver entry points
    #

    @catch_test_errors
    def tear_up(self):
        super(BuildAndRunDriver, self).tear_up()

        project_file = self.test_env.get('project_file', None)
        main = self.test_env.get('main', None)

        if not project_file or not isinstance(project_file, basestring):
            raise SetupError('test.yaml: please define a "project_file" string'
                             ' field')
        if not main or not isinstance(main, basestring):
            raise SetupError('test.yaml: please define a "main" string field')

        self.project_file = project_file
        self.main_program = main
        self.fake_ada_target = self.test_env.get('fake_ada_target', None)

    @catch_test_errors
    def run(self):
        # If we are requested to run with a fake toolchain, set it up now
        if self.fake_ada_target:
            fake_dir = self.working_dir('fake-ada')
            create_fake_ada_compiler(
               comp_dir=fake_dir, comp_target=self.fake_ada_target,
               gnat_version="21.0w", gcc_version="8.4.3", runtimes=["rtp"],
               comp_is_cross=True)
            Env().add_path(os.path.join(fake_dir, 'bin'))

        # Build the program and run it
        self.run_and_check(['gprbuild', '-g1', '-q', '-p',
                            '-P', self.project_file, '-bargs', '-Es'])
        self.run_and_check((
           ['valgrind', '-q'] if self.global_env['options'].valgrind else []) +
           [os.path.join('.', self.main_program)])
