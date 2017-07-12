import os
import os.path

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, SetupError,
)


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

    @catch_test_errors
    def run(self):
        # Make the "gpr2.gpr" project file available to testcases
        gpr_path = os.path.abspath(os.path.join(
            self.testsuite_dir,
            '..'
        ))
        old_path = os.environ.get('GPR_PROJECT_PATH', '')
        os.environ['GPR_PROJECT_PATH'] = (
            gpr_path + os.pathsep + old_path
            if old_path else
            gpr_path
        )

        # Then, build the program and run it
        self.run_and_check(['gprbuild', '-q', '-p', '-P', self.project_file])
        self.run_and_check([os.path.join('.', self.main_program)])
