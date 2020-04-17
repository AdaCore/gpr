import os

from e3.env import Env
import e3.testsuite

from testsuite_support.build_and_run import BuildAndRunDriver
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

    def set_up(self):
        super(Testsuite, self).set_up()

        Env().add_search_path('PYTHONPATH',
                              os.path.join(self.root_dir, 'testsuite_support'))
        self.env.valgrind = self.main.args.valgrind
