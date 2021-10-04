from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN
from os import environ


br = BuilderAndRunner()
environ["LIBRARY_TYPE"] = "relocatable"
br.run(['gprbuild', '-p', '-q', '-Pwrapper.gpr'])
br.call([GPRCLEAN])
