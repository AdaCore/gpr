from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS

BuilderAndRunner().call([GPRLS, "-Pfiles/prj1"])
BuilderAndRunner().call([GPRLS, "-Pfiles/prj2"])
BuilderAndRunner().call([GPRLS, "-Pfiles/prj3"])
BuilderAndRunner().call([GPRLS, "-Pfiles/prj4"])
BuilderAndRunner().call([GPRLS, "-Pfiles/prj5"])
