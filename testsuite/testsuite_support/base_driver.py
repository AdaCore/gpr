import os
import os.path
import sys
import string

from gnatpython import fileutils
from gnatpython.arch import Arch
from gnatpython.ex import Run, STDOUT
from gnatpython.fileutils import mkdir
from gnatpython.testsuite.driver import TestDriver


class SetupError(Exception):
    """Exception to raise when the testcase is invalid.

    Helper exception to work with catch_test_errors: see below.
    """
    pass


class TestError(Exception):
    """Exception to raise when the testcase fails.

    Helper exception to work with catch_test_errors: see below.
    """
    pass


def catch_test_errors(func):
    """
    Helper decorator for driver entry points.

    This returns a wrapper around func that catches SetupError and TestError
    exceptions and that turns them into the appropriate test status. Using
    exceptions is convenient to stop any method from any point: this simplifies
    the control flow.
    """

    def wrapper(self, *args, **kwargs):
        try:
            return func(self, *args, **kwargs)
        except SetupError as exc:
            self.set_setup_error(exc.message)
        except TestError as exc:
            self.set_failure(exc.message)
    return wrapper


# create_fake_ada_compiler routine copied from gprbuild-internal testsuite
# support code.

def create_fake_ada_compiler(comp_dir, comp_target, gnat_version,
                             gcc_version, comp_is_cross=False,
                             runtimes=["native", "sjlj"],
                             create_symlink=False,
                             create_ada_object_path=False):
    """
       Create directory defined by the comp_dir parameter and put fake Ada
       compiler directory tree there. If comp_is_cross is true, the compiler
       tools 'gnatmake', 'gcc', and 'gnatls' will be prefixed by the
       comp_target. If create_symlink is true, the first runtime from the
       runtimes will be made available as default through an 'adalib' symbolic
       link.
       If create_ada_object_path is true, that file will be created to simulate
       a Windows install.
    """

    if comp_is_cross:
        comp_prefix = comp_target + '-'
    else:
        comp_prefix = ""

    arch = Arch()
    comp_dict = {'comp_target': comp_target,
                 'gnat_version': gnat_version,
                 'gcc_version': gcc_version,
                 'comp_prefix': comp_prefix,
                 'exeext': arch.os.exeext}

    mkdir(os.path.join(comp_dir, 'bin'))
    gnatls_adb = open(os.path.join(comp_dir, 'bin', 'gnatls.adb'), 'w')
    gnatls_adb.write("""
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure gnatls is
begin
   if Argument_Count >= 1 and Argument (1) = "-v" then
        Put_Line ("GNATLS Pro %(gnat_version)s (20190507-89)");
   else
         Put ("Running gnatls");
         for J in 1 .. Argument_Count loop
             Put (" " & Argument (J));
         end loop;
   end if;
end gnatls;
""" % comp_dict)
    gnatls_adb.close()

    gcc_adb = open(os.path.join(comp_dir, 'bin', 'gcc.adb'), 'w')
    gcc_adb.write("""
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure gcc is
begin
   if Argument_Count >= 1 and then Argument (1) = "-v" then
        Put_Line ("gcc version %(gcc_version)s 20131008 for GNAT Pro");
   elsif Argument_Count >= 1 and then Argument (1) = "--version" then
        Put_Line ("gcc (GCC) %(gcc_version)s");
   elsif Argument_Count >= 1 and then Argument (1) = "-dumpmachine" then
        Put_Line ("%(comp_target)s");
   else
         Put ("Running gcc");
         for J in 1 .. Argument_Count loop
             Put (" " & Argument (J));
         end loop;
   end if;
end gcc;
""" % comp_dict)
    gcc_adb.close()

    gnatmake_adb = open(os.path.join(comp_dir, 'bin', 'gnatmake.adb'), 'w')
    gnatmake_adb.write("""
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure gnatmake is
begin
         Put ("Running gcc");
         for J in 1 .. Argument_Count loop
             Put (" " & Argument (J));
         end loop;
end gnatmake;
""")
    gnatmake_adb.close()

    for tool in ['gnatmake', 'gcc', 'gnatls']:
        comp_dict['bin'] = tool

        # Do not run gnatmake in the same directory with the fake tools sources
        # to avoid using just created fake tools in the build process.

        Run(['gnatmake', os.path.join('bin', tool + '.adb'), '-o',
             os.path.join('bin',
                          '%(comp_prefix)s%(bin)s%(exeext)s' % comp_dict)],
            cwd=comp_dir)

    if comp_target == "dotnet":
        for dir in ("adalib", "adainclude"):
            mkdir(os.path.join(comp_dir, 'lib', 'dotgnat', dir))
    else:
        for runtime in runtimes:
            for dir in ("adalib", "adainclude"):
                mkdir(os.path.join(comp_dir, 'lib', 'gcc', comp_target,
                                   gcc_version, 'rts-%s' % runtime, dir))

    libdir = os.path.join(comp_dir, 'lib', 'gcc', comp_target, gcc_version)

    # On Unix systems, we have a symbolic link for the default
    # runtime. gprconfig should automatically detect these are
    # the same two runtimes and only list "native".

    if create_symlink:
        os.symlink(
            os.path.join('rts-%s' % runtimes[0], 'adalib'),
            os.path.join(libdir, 'adalib'))

    # Simulate windows system, with an ada_object_path file

    if create_ada_object_path:
        with open(os.path.join(libdir, 'ada_object_path'), 'w') as ada_obj:
            ada_obj.write("rts-%s/adalib" % runtimes[0])


class BaseDriver(TestDriver):
    """
    Base class to provide common test driver helpers.

    Ideally, these should end up in GNATpython, but this base class acts as a
    staging area: once it has been proven that some feature is useful, it may
    be easier to submit it upstream...
    """

    TIMEOUT = None

    def tear_up(self):
        super(BaseDriver, self).tear_up()
        self.create_test_workspace()

        try:
            _ = self.test_env['description']
        except KeyError:
            print(_)
            raise SetupError('test.yaml: missing "description" field')

        self.check_file(self.expected_file)

        # See if we expect a failure for this testcase
        try:
            comment = self.test_env['expect_failure']
        except KeyError:
            self.expect_failure = False
            self.expect_failure_comment = None
        else:
            # Because of wrapping in the YAML file, we can get multi-line
            # strings, which is not valid for comments.
            comment = comment.replace('\n', ' ').strip()

            self.expect_failure = True
            if not (comment is None or isinstance(comment, basestring)):
                raise SetupError('Invalid "expect_failure" entry:'
                                 ' expected a string but got {}'.format(
                                     type(comment)))
            self.expect_failure_comment = comment

    def read_file(self, filename):
        """Return the content of `filename`."""
        with open(filename, 'r') as f:
            return f.read()

    def set_setup_error(self, message):
        self.result.set_status('PROBLEM', message)

    def set_failure(self, message):
        if self.expect_failure:
            self.result.set_status('XFAIL', '{}{}'.format(
                message,
                ' ({})'.format(self.expect_failure_comment)
                if self.expect_failure_comment else ''
            ))
        else:
            self.result.set_status('FAILED', message)

    def set_passed(self):
        if self.expect_failure:
            msg = (
                'Failure was expected: {}'.format(self.expect_failure_comment)
                if self.expect_failure_comment else None
            )
            self.result.set_status('UOK', msg)
        else:
            self.result.set_status('PASSED')

    # Convenience path builders

    @property
    def testsuite_dir(self):
        """Return the absolute path to the testsuite root directory."""
        result = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              '..')
        return os.path.abspath(result)

    @property
    def test_dir(self):
        """Return the path of the current testcase directory."""
        return self.test_env['test_dir']

    def working_dir(self, *args):
        """
        Return the working dir, plus any path elements joined to it if passed
        in *args.
        """
        return os.path.join(self.global_env['working_dir'],
                            self.test_env['test_name'], *args)

    @property
    def output_file(self):
        return self.working_dir('actual.out')

    @property
    def expected_file(self):
        return self.working_dir('test.out')

    @property
    def original_expected_file(self):
        return os.path.join(self.test_dir, 'test.out')

    #
    # Tear up helpers
    #

    def check_file(self, filename):
        """
        Check file presence.

        If the file does not exist test is aborted.
        """
        if not os.path.isfile(os.path.join(self.test_dir, filename)):
            raise SetupError('Missing mandatory file: {}'.format(filename))

    def create_test_workspace(self):
        """
        Create a test workspace.

        This function copies the test sources into the working directory.
        """
        fileutils.sync_tree(self.test_dir, self.working_dir())

    #
    # Run helpers
    #

    def run_and_check(self, argv):
        """
        Run a subprocess with `argv` and check it completes with status code 0.

        In case of failure, the test output is appended to the actual output
        and a TestError is raised.
        """
        program = argv[0]

        p = Run(argv, cwd=self.working_dir(),
                timeout=self.TIMEOUT,
                output=self.output_file,
                error=STDOUT)

        if p.status != 0:
            self.result.actual_output += (
                '{} returned status code {}\n'.format(program, p.status))
            self.result.actual_output += self.read_file(self.output_file)
            raise TestError(
                '{} returned status code {}'.format(program, p.status))

        #  convert Windows directory separators to expected one
        if sys.platform == 'win32':
            content = string.replace(self.read_file(self.output_file),
                                     '\\', '/')
            with open(self.output_file, 'w') as f:
                return f.write(content)

    #
    # Analysis helpers
    #

    def analyze(self):
        # Check for the test output itself
        diff = fileutils.diff(self.expected_file, self.output_file,
                              ignore_white_chars=False)
        if diff:
            self.set_failure('output is not as expected')
            self.result.actual_output += diff
        else:
            self.set_passed()


# filter routine replaces in output all actual occurrences by expected.

def filter(output, actual, expected):
    return output.replace(actual, expected)


# filter_cwd replaces in output all
# current working directory occurrences by ''

def filter_cwd(output):
    return output.replace(os.getcwd(), '')
