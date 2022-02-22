import os
import sys

from testsuite_support.base_driver import BaseDriver
from testsuite_support.builder_and_runner import BuilderAndRunner


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

    def run(self):
        builder_and_runner = BuilderAndRunner(self)

        cmd = [sys.executable, 'test.py']

        env = dict(os.environ)
        if 'PYTHONPATH' not in os.environ:
            env['PYTHONPATH'] = self.env.root_dir
        else:
            env['PYTHONPATH'] = str(self.env.root_dir) + \
                                os.path.pathsep + os.environ['PYTHONPATH']
        builder_and_runner.insert_build_and_runner_parameters(env)

        env["root_dir"] = self.env.root_dir

        self.shell(cmd, env=env)
