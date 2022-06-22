import os
from e3.os.process import PIPE, Run, STDOUT
from random import getrandbits
from e3.testsuite.driver.classic import TestAbortWithFailure

# environment variables definition

USE_VALGRIND = "USE_VALGRIND"
USE_GNATCOV = "USE_GNATCOV"
COV_TRACES_DIR = "GNATCOV_TRACES_DIR"
COV_LEVEL = "GNATCOV_LEVEL"

GPR = os.environ["GPR2_TOOLS_PREFIX"] if "GPR2_TOOLS_PREFIX" in os.environ else "gpr"
GPRLS = GPR + "ls"
GPRCLEAN = GPR + "clean"
GPRINSTALL = GPR + "install"
GPRNAME = GPR + "name"
GPRCONFIG = GPR + "config"
GPRREMOTE = GPR + "remote"
GPRDOC = GPR + "doc"
GPRINSPECT = GPR + "inspect"


class BuilderAndRunner(object):
    """
    Handle program using gpr2 build. in coverage mode, instrument code &
    build instrumented code.

    Run program using gpr2 (including prebuilt gpr2-tools) using valgrind when
    requested.

    When using BuilderAndRunner() object is configured using python script
    parameters --valgrind --gnatcov --level --traces-dir

    In coverage mode, gpr2 programs's coverage traces are written in
    traces-dir using a random filename.

    This object provides wrappers for e3.os.process.Run, subprocess.call &
    subprocess.check_output
    """

    def __init__(self, driver=None):
        """configure BuilderAndRunner object using TestDriver environment or
        python script's argument when driver is None.
        """

        # associated TestDriver object
        self.driver = driver

        if driver is not None:

            # valgrind mode enabled status
            self.valgrind = driver.env.valgrind

            # coverage mode enabled status
            self.gnatcov = driver.env.gnatcov is not None

            if self.gnatcov:
                # gnatcov trace files directory
                self.traces_dir = driver.env.gnatcov.traces_dir

                # gnatcov calls --level switch
                self.level = driver.env.gnatcov.covlevel
            else:
                self.level = None
                self.traces_dir = None
        else:
            self.valgrind = USE_VALGRIND in os.environ
            self.gnatcov = USE_GNATCOV in os.environ
            self.traces_dir = os.environ.get(COV_TRACES_DIR)
            self.level = os.environ.get(COV_LEVEL)

    def simple_run(
        self,
        cmd,
        env=None,
        catch_error=True,
        output=PIPE,
        error=STDOUT,
        analyze_output=True,
    ):
        """ generic TestDriver.shell or e3.os.process.Run runner"""
        if self.driver is not None:
            if env is not None:
                effective_env = dict(os.environ)
                effective_env.update(env)
            return self.driver.shell(
                cmd,
                env=effective_env,
                catch_error=catch_error,
                analyze_output=analyze_output,
            )
        else:
            p = Run(cmd, env=env, output=output, error=error, ignore_environ=False)
            if catch_error and p.status != 0:
                print(str(cmd) + " returned " + str(p.status))
                if p.out:
                    print("stdout\n" + p.out)
                if p.err:
                    print("stderr\n" + p.err)
                raise TestAbortWithFailure("non-zero exit status")
            else:
                return p

    def build(self, project, vars=[], args=[], env=None, output=PIPE):
        """ gprbuild wrapper for normal & coverage modes """
        # If code coverage is requested, leave a chance to gnatcov to decorate
        # the execution of the subprogram in order to make it contribute to
        # code coverage.
        if self.gnatcov:
            gnatcov_cmd = [
                "gnatcov",
                "instrument",
                "--level",
                self.level,
                "--dump-trigger=atexit",
                "--externally-built-projects",
                "--projects",
                "gpr2",
                "--no-subprojects",
                "-P",
                project,
            ] + vars
            self.simple_run(gnatcov_cmd, env=env, analyze_output=False)

            # to do coverage, we use the repo's gpr2 project file instead of
            # the installed one from libgpr2. This means we need to ensure
            # that some scenario variables are properly set.
            gprbuild_cmd = (
                ["gprbuild", "-P", project,
                 "-XGPR2_BUILD=gnatcov", "-XXMLADA_BUILD=static"]
                + vars
                + ["--src-subdirs=gnatcov-instr", "--implicit-with=gnatcov_rts"]
                + args
            )
        else:
            gprbuild_cmd = ["gprbuild", "-P", project] + vars + args

        return self.simple_run(gprbuild_cmd, env=env, output=output)

    def run(self, cmd, env=None, output=PIPE, catch_error=False):
        """ generic wrapper handling coverage & valgrind modes. """
        if self.gnatcov:
            if env is None:
                env = {}
            file = str(getrandbits(128)) + ".srctrace"
            env["GNATCOV_TRACE_FILE"] = os.path.join(self.traces_dir, file)
        run_cmd = (["valgrind", "-q"] if self.valgrind else []) + cmd

        return self.simple_run(run_cmd, env=env, catch_error=catch_error, output=output)

    def check_output(self, cmd):
        """subprocess.check_output wrapper handling coverage & valgrind
        modes.
        """
        return self.run(cmd, catch_error=True)

    def call(self, cmd):
        """subprocess.call wrapper handling coverage & valgrind
        modes.
        """
        p = self.run(cmd)
        print(p.out, end="")
        return p

    def check_call(self, cmd):
        """subprocess.call wrapper handling coverage & valgrind
        modes.
        """
        p = self.run(cmd, catch_error=True)
        print(p.out, end="")
        return p

    def insert_build_and_runner_parameters(self, env):
        """The BuildAndRunner initialization parameters to allow a
        BuilderAndRunner() call in the python test script.
        """
        if self.valgrind:
            env[USE_VALGRIND] = "true"
        if self.gnatcov:
            env[USE_GNATCOV] = "true"
            env[COV_TRACES_DIR] = self.traces_dir
            env[COV_LEVEL] = self.level
