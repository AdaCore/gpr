import glob
import json
import os.path

from e3.env import Env
from e3.fs import ls, mkdir, sync_tree
from e3.os.fs import which

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
env = Env()


def find_rts():
    gnat = which("gcc")
    assert gnat is not None, "cannot find gnat"
    install_dir = os.path.dirname(os.path.dirname(gnat))

    lib_dir = glob.glob(os.path.join(install_dir, "lib", "gcc", "*", "*"))
    assert len(lib_dir) == 1, "issue finding the runtime dir"
    lib_dir = lib_dir[0]

    if env.host.os.name != "windows":
        lib_dir = os.path.join(lib_dir, "rts-native")

    return lib_dir


# lookup for the native runtime dir
native_rts = find_rts()

# install the sources locally
local_rts = os.path.join(os.getcwd(), "build")
mkdir(local_rts)

# those must be in an adainclude dir
adainclude = os.path.join(local_rts, "adainclude")
sync_tree(os.path.join(native_rts, "adainclude"), adainclude)

# ??? patch libgnarl.gpr so that it depends on libgnat.gpr. Temporary solution
# until the project actually depends on libgnat
with open(os.path.join(adainclude, "libgnarl.gpr")) as fp:
    cnt = fp.read().splitlines()
if 'with "libgnat";' not in cnt:
    cnt.insert(0, 'with "libgnat";')
    with open(os.path.join(adainclude, "libgnarl.gpr"), "w") as fp:
        fp.write("\n".join(cnt))

# create the library_dir, else gnat will complain (??? it should be created by gprbuild beforehand?)
mkdir(os.path.join(local_rts, "adalib"))

# build libgnat then libgnarl
for lib in "libgnat", "libgnarl":
    print(f"BUILD {lib}...")
    bnr.check_output(
        [
            GPRBUILD,
            f"-P{adainclude}/{lib}.gpr",
            "-p",
            "-q",
            "--json-summary",
            "--keep-temp-files",
            "-XLIBRARY_KIND=relocatable",
            f"--RTS={local_rts}",
        ]
    )

    print("check args during link...")
    summary = os.path.join(adainclude, "jobs.json")

    with open(summary) as fp:
        cnt = json.load(fp)

    dep_libs = {}

    for action in cnt:
        if action["uid"].startswith("[Link]") and action["uid"].find(lib) > 0:
            # extract the response file used during the link if any
            resp_file = None

            for arg in action["command"].split(" "):
                if arg.startswith("@"):
                    # a response file is used
                    resp_file = arg[1:]
                    break
                elif arg.startswith("-l"):
                    # otherwise add the dependency libs directly
                    dep_libs[arg] = None
            if resp_file is not None:
                with open(resp_file) as fp:
                    cnt = fp.read().splitlines()
                for arg in cnt:
                    # get dependency libs from the response file
                    if arg.startswith("-l"):
                        dep_libs[arg] = None
            break
    print(f"{lib} library deps: " + " ".join(sorted(dep_libs.keys())))

    # check the libs don't depend on external libgnat/libgnarl

    has_libgnat = False

    # ... except on x86-windows where the dlls always hold a ref to themselves
    # for some reason

    if env.host.platform != "x86-windows":
        lib_ext = ".dll" if env.host.os.name == "windows" else ".so"

        result = bnr.simple_run(
            ["ldd", os.path.join(local_rts, "adalib", f"{lib}{lib_ext}")]
        ).out

        for l in result.splitlines():
            l = l.strip()
            if l.startswith("libgnarl"):
                print("error, dependency on libgnat/libgnarl detected")
                print(l)
                has_libgnat = True
            elif l.startswith("libgnat") and lib == "libgnat":
                print("error, libgnat depends on libgnat")
                print(l)
                has_libgnat = True

    if not has_libgnat:
        print("no dependency on libgnat/libgnarl, good")
