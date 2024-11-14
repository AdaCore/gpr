from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS

BuilderAndRunner().call([GPRLS, "-Pimp4/imp4"])
