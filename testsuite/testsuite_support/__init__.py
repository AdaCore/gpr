import e3.testsuite

from testsuite_support.build_and_run import BuildAndRunDriver
from testsuite_support.gnatcov import GNATcov
from testsuite_support.python_script import PythonScriptDriver


class Testsuite(e3.testsuite.Testsuite):
    tests_subdir = 'tests'
    enable_cross_support = True
    test_driver_map = {
        'build_and_run': BuildAndRunDriver,
        'python_script': PythonScriptDriver,
    }

    def add_options(self, parser):
        parser.add_argument(
            "--valgrind",
            action="store_true",
            help="Run test executable under valgrind.")

        parser.add_argument(
            "--gnatcov",
            help="If provided, compute the source code coverage of testcases"
                 " on ALS. This requires GNATcoverage working with"
                 " instrumentation. The argument passed must be a directory"
                 " that contains all SID files.")

        parser.add_argument(
            "--level",
            help="coverage level. can be:"
                 " branch, insn, stmt, stmt+decision, stmt+mcdc, stmt+uc_mcdc")

    def set_up(self):
        super(Testsuite, self).set_up()
        self.env.valgrind = self.main.args.valgrind

        # If code coverage is requested, initialize our helper and build
        # instrumented programs.
        self.env.gnatcov = (GNATcov(self) if self.main.args.gnatcov else None)
        self.env.root_dir = self.root_dir

    def tear_down(self):
        if self.env.gnatcov:
            self.env.gnatcov.report()

        super(Testsuite, self).tear_down()
