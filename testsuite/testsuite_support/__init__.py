import os

from gnatpython.testsuite import Testsuite as BaseTestsuite

from testsuite_support.build_and_run import BuildAndRunDriver
from testsuite_support.python_script import PythonScriptDriver


class Testsuite(BaseTestsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {
        'build_and_run': BuildAndRunDriver,
        'python_script': PythonScriptDriver,
    }
