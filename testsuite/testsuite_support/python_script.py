import sys

from testsuite_support.base_driver import BaseDriver


class PythonScriptDriver(BaseDriver):
    """
    Driver to run a Python script.

    Interface:

    * put a "test.py" script in the test directory;
    * put a "test.out" text file in the test directory.

    This driver will run the Python script. Its output is then checked against
    the expected output (test.out file). This mechanism is the most flexible
    way to write a testcase, but also the more verbose one and the most complex
    one. Use this driver when no other one fits.
    """

    default_process_timeout = 300

    def run(self):
        self.shell([sys.executable, 'test.py'])
