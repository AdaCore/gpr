from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRCLEAN, GPRBUILD

from os import environ


br = BuilderAndRunner()
environ["LIBRARY_TYPE"] = "relocatable"
br.run([GPRBUILD, '-p', '-q', '-Pwrapper.gpr'])
br.call([GPRCLEAN])
