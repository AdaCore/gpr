import os
import shlex
from e3.os.process import PIPE, Run, STDOUT
from random import getrandbits
from e3.testsuite.driver.classic import TestAbortWithFailure
from testsuite_support.tools import GPRBUILD, GPR2BUILD, GPR2BUILD_NAME, GPR2CLEAN_NAME, GPRINSTALL, GPRLS

# environment variables definition

USE_VALGRIND = "USE_VALGRIND"
USE_GNATCOV = "USE_GNATCOV"
COV_TRACES_DIR = "GNATCOV_TRACES_DIR"
COV_LEVEL = "GNATCOV_LEVEL"

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

            # Use gpr2build instead of gprbuild
            self.use_gpr2build = driver.env.use_gpr2build

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
            self.use_gpr2build = False

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
            effective_env = dict(os.environ)
            if env is not None:
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

    def build(self, project, vars=[], args=[], env=None, output=PIPE, use_gpr2build=False):
        """ gprbuild wrapper for normal & coverage modes """

        gprbuild = GPR2BUILD if use_gpr2build else GPRBUILD

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
                [gprbuild, "-P", project,
                 "-XGPR2_BUILD=gnatcov", "-XXMLADA_BUILD=static"]
                + vars
                + ["--src-subdirs=gnatcov-instr", "--implicit-with=gnatcov_rts"]
                + args
            )
        else:
            gprbuild_cmd = [gprbuild, "-P", project] + vars + args

        return self.simple_run(gprbuild_cmd, env=env, output=output)

    def run(self, cmd, env=None, output=PIPE, catch_error=False):
        """ generic wrapper handling coverage & valgrind modes. """
        if self.gnatcov:
            if env is None:
                env = {}
            file = str(getrandbits(128)) + ".srctrace"
            env["GNATCOV_TRACE_FILE"] = os.path.join(self.traces_dir, file)

            # For debuggability, add metadata so that it is possible to know
            # which test/command produced which source trace.
            with open(
                os.path.join(self.traces_dir, file + "-context.txt"), "w"
            ) as f:
                print("CMD:", shlex.join(cmd), file=f)
                print("PWD:", os.getcwd(), file=f)

        if self.valgrind:
            run_cmd = (
                ["valgrind", "-q", "--leak-check=full", "--show-possibly-lost=no",
                 f"--suppressions={os.path.dirname(os.path.dirname(__file__))}/valgrind-suppressions.txt"]
                 + cmd
                )
        else:
            run_cmd = cmd

        return self.simple_run(run_cmd, env=env, catch_error=catch_error, output=output)

    def check_output(self, cmd):
        """subprocess.check_output wrapper handling coverage & valgrind
        modes.
        """
        return self.run(cmd, catch_error=True)

    def call(self, cmd, quiet=False):
        """subprocess.call wrapper handling coverage & valgrind
        modes.
        """
        p = self.run(cmd)
        if not quiet:
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
        if self.use_gpr2build:
            # the tested gpr2 package creates gpr2build as "gprbuild"
            env[GPR2BUILD_NAME] = "gprbuild"
            env[GPR2CLEAN_NAME] = "gprclean"
        if self.gnatcov:
            env[USE_GNATCOV] = "true"
            env[COV_TRACES_DIR] = self.traces_dir
            env[COV_LEVEL] = self.level
