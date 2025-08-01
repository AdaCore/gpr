import os
import os.path
import re
from subprocess import check_output
from typing import NoReturn

from e3.env import Env
from e3.fs import mkdir
from e3.os.process import which
from e3.os.fs import unixpath
from e3.testsuite.driver.classic import TestAbortWithError
from e3.testsuite.driver.diff import (
    DiffTestDriver,
    ReplacePath,
    Substitute,
    PatternSubstitute,
    RefiningChain,
)


# create_fake_ada_compiler routine copied from gprbuild-internal testsuite
# support code.


def create_fake_ada_compiler(
    driver,
    comp_dir,
    comp_target,
    gnat_version,
    gcc_version,
    comp_is_cross=False,
    runtimes=["native", "sjlj"],
    create_symlink=False,
    create_ada_object_path=False,
):
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
        comp_prefix = comp_target + "-"
    else:
        comp_prefix = ""

    env = Env()
    comp_dict = {
        "comp_target": comp_target,
        "gnat_version": gnat_version,
        "gcc_version": gcc_version,
        "comp_prefix": comp_prefix,
        "exeext": env.build.os.exeext,
    }

    mkdir(os.path.join(comp_dir, "bin"))
    gnatls_adb = open(os.path.join(comp_dir, "bin", "gnatls.adb"), "w")
    gnatls_adb.write(
        """
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
"""
        % comp_dict
    )
    gnatls_adb.close()

    gcc_adb = open(os.path.join(comp_dir, "bin", "gcc.adb"), "w")
    gcc_adb.write(
        """
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
"""
        % comp_dict
    )
    gcc_adb.close()

    gnatmake_adb = open(os.path.join(comp_dir, "bin", "gnatmake.adb"), "w")
    gnatmake_adb.write(
        """
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure gnatmake is
begin
         Put ("Running gcc");
         for J in 1 .. Argument_Count loop
             Put (" " & Argument (J));
         end loop;
end gnatmake;
"""
    )
    gnatmake_adb.close()

    for tool in ["gnatmake", "gcc", "gnatls"]:
        comp_dict["bin"] = tool

        # Do not run gnatmake in the same directory with the fake tools sources
        # to avoid using just created fake tools in the build process.

        driver.shell(
            [
                "gnatmake",
                os.path.join("bin", tool + ".adb"),
                "-o",
                os.path.join("bin", "%(comp_prefix)s%(bin)s%(exeext)s" % comp_dict),
            ],
            cwd=comp_dir,
            analyze_output=False,
        )

    if comp_target == "dotnet":
        create_fake_ada_runtime(os.path.join(comp_dir, "lib", "dotgnat"))
    else:
        for runtime in runtimes:
            create_fake_ada_runtime(
                os.path.join(
                    comp_dir, "lib", "gcc", comp_target, gcc_version, "rts-%s" % runtime
                )
            )

    libdir = os.path.join(comp_dir, "lib", "gcc", comp_target, gcc_version)

    # On Unix systems, we have a symbolic link for the default
    # runtime. gprconfig should automatically detect these are
    # the same two runtimes and only list "native".

    if create_symlink:
        os.symlink(
            os.path.join("rts-%s" % runtimes[0], "adalib"),
            os.path.join(libdir, "adalib"),
        )

    # Simulate windows system, with an ada_object_path file

    if create_ada_object_path:
        with open(os.path.join(libdir, "ada_object_path"), "w") as ada_obj:
            ada_obj.write("rts-%s/adalib" % runtimes[0])


def create_fake_ada_runtime(path):
    for dir in ("adalib", "adainclude"):
        mkdir(os.path.join(path, dir))


class BaseDriver(DiffTestDriver):
    """Base class to provide common test driver helpers."""

    def create_fake_ada_compilers(self, env={}):
        if isinstance(self.fake_ada_target, list):
            targets = self.fake_ada_target
        else:
            targets = [self.fake_ada_target]
        paths = []
        for tgt_rts in targets:
            tgt = tgt_rts[0]
            rts = tgt_rts[1:]
            fake_dir = self.working_dir("fake-ada-%s" % tgt)
            paths.append(os.path.join(fake_dir, "bin"))

            if "linux" in tgt or "windows" in tgt:
                is_cross = False
            elif tgt == "native":
                is_cross = False
                tgt = Env().host.triplet
            else:
                is_cross = True
            if not is_cross:
                rts = ["native", "sjlj", "light"]

            create_fake_ada_compiler(
                self,
                comp_dir=fake_dir,
                comp_target=tgt,
                gnat_version="21.0w",
                gcc_version="8.4.3",
                runtimes=rts,
                comp_is_cross=is_cross,
            )

        paths.append(os.environ.get("PATH"))
        env["PATH"] = os.pathsep.join(paths)

    def set_up(self):
        super(BaseDriver, self).set_up()
        self.fake_ada_target = self.test_env.get("fake_ada_target")

        description = self.test_env.get("description", None)
        if description is None:
            raise TestAbortWithError('test.yaml: missing "description" field')
        output = self.test_env.get("output", None)
        if output is not None:
            test_out = None
            if not isinstance(output, list):
                raise TestAbortWithError('test.yaml: "output" field must be a list')

            for i, entry in enumerate(output, 1):

                def error(message: str) -> NoReturn:
                    raise TestAbortWithError(
                        "test.yaml: output.entry#{}: {}".format(i, message)
                    )

                if (
                    not isinstance(entry, list)
                    or len(entry) != 2
                    or any(not isinstance(s, str) for s in entry)
                ):
                    error("list of 2 strings expected")
                try:
                    cond_env = dict()
                    cond_env["env"] = self.env
                    cond = eval(entry[1], cond_env)
                except Exception as exc:
                    error("invalid condition ({}): {}".format(type(exc).__name__, exc))
                if cond:
                    if test_out is not None:
                        error(
                            f"several test.out selected: {test_out} and {entry[0]}"
                        )
                    test_out = entry[0]
            if test_out is not None:
                self.test_env["baseline_file"] = test_out

    @property
    def output_refiners(self):
        class ReplacePathIgnoreCasing(RefiningChain[str]):
            """Return an output refiner that replaces the given path, regardless of its case."""

            def __init__(self, path: str, replacement: str = "") -> None:
                # First replace the normalized path, then the Unix-style path (which
                # some tool may output even on Windows) and finally the very path that
                # was given.

                def get_substitute_for(pattern: str):
                    substitute_ignore_casing = PatternSubstitute(
                        pattern=pattern,
                        replacement=replacement,
                    )
                    # Manually set the case-insensitive flag
                    substitute_ignore_casing.regexp = re.compile(pattern, re.IGNORECASE)
                    return substitute_ignore_casing

                super().__init__(
                    [
                        get_substitute_for(r"%s" % re.escape(substring))
                        for substring in [os.path.realpath(path), unixpath(path), path]
                    ]
                )

        # Find gcc
        gcc = which("gcc")
        _, gcc_install = os.path.splitdrive(os.path.dirname (os.path.dirname (gcc)).lower())

        # and figure out gcc version
        out = check_output([gcc, "--version"]).decode().splitlines()[0]
        gcc_version = re.sub(r'gcc(.EXE)? \(GCC\) ([0-9.]*)\s.*', r'\2', out)

        # Remove working directory from output and
        # make all filenames look like Unix ones (forward slashes for directory
        # separators, no drive letter).

        def to_lower(match: re.Match) -> str:
            return match.group(0).lower()

        return super().output_refiners + [
            ReplacePath(self.working_dir(), replacement=""),
            ReplacePathIgnoreCasing(gcc_install, replacement="<gcc>"),
            Substitute("c:<gcc>", replacement="<gcc>"),
            Substitute("\\", "/"),
            ReplacePath("C:/", "/"),
            Substitute(".exe", ""),
            Substitute(gcc_version, "(gcc-version)"),
            Substitute("gpr2build", replacement="gprbuild"),
            Substitute("gpr2clean", replacement="gprclean"),
            Substitute("aarch64-linux-gnu", replacement="(host)"),
            Substitute("x86_64-pc-linux-gnu", replacement="(host)"),
            Substitute("i686-pc-linux-gnu", replacement="(host)"),
            Substitute("x86_64-w64-mingw32", replacement="(host)"),
            Substitute("i686-pc-mingw32", replacement="(host)"),
            Substitute("aarch64-linux", replacement="(host)"),
            Substitute("x86_64-linux", replacement="(host)"),
            Substitute("x86-linux", replacement="(host)"),
            Substitute("x86_64-windows64", replacement="(host)"),
            Substitute("x86_64-windows", replacement="(host)"),
            Substitute("x86-windows", replacement="(host)"),
            PatternSubstitute(r"[.]text\+0x[0-9a-f]+", ".text+0x<nn>"),
            PatternSubstitute(r"_.{13}\.tmp", ".tmp"),
        ]

    # Convenience path builders

    @property
    def testsuite_dir(self):
        """Return the absolute path to the testsuite root directory."""
        result = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..")
        return os.path.abspath(result)
