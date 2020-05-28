import os
import os.path

from e3.testsuite.driver.classic import TestAbortWithError

from testsuite_support.base_driver import BaseDriver, create_fake_ada_compiler


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

    default_process_timeout = 300

    def set_up(self):
        super(BuildAndRunDriver, self).set_up()

        project_file = self.test_env.get('project_file', None)
        main = self.test_env.get('main', None)

        if not project_file or not isinstance(project_file, str):
            raise TestAbortWithError(
                'test.yaml: please define a "project_file" string field')
        if not main or not isinstance(main, str):
            raise TestAbortWithError(
                'test.yaml: please define a "main" string field')

        self.project_file = project_file
        self.main_program = main
        self.fake_ada_target = self.test_env.get('fake_ada_target', None)

    def run(self):
        env = dict(os.environ)

        # If we are requested to run with a fake toolchain, set it up now
        if self.fake_ada_target:
            fake_dir = self.working_dir('fake-ada')
            create_fake_ada_compiler(
               self,
               comp_dir=fake_dir, comp_target=self.fake_ada_target,
               gnat_version="21.0w", gcc_version="8.4.3", runtimes=["rtp"],
               comp_is_cross=True)
            env['PATH'] = (
                os.path.join(fake_dir, 'bin') + os.path.pathsep + env['PATH'])

        # Build the program and run it
        self.shell(['gprbuild', '-g1', '-q', '-p', '-P', self.project_file,
                    '-bargs', '-Es'],
                   env=env)
        self.shell(
           (['valgrind', '-q'] if self.env.valgrind else []) +
           [os.path.join('.', self.main_program)],
           env=env)
