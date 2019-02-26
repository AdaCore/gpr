import os

from gnatpython.testsuite import Testsuite as BaseTestsuite

from testsuite_support.build_and_run import BuildAndRunDriver
from testsuite_support.python_script import PythonScriptDriver


class Testsuite(BaseTestsuite):
    TEST_SUBDIR = 'tests'
    CROSS_SUPPORT = True
    DRIVERS = {
        'build_and_run': BuildAndRunDriver,
        'python_script': PythonScriptDriver,
    }
    def add_options(self):
        self.main.add_option(
            "--valgrind",
            dest="valgrind",
            action="store_true",
            default=False,
            help="Run test executable under valgrind.")
