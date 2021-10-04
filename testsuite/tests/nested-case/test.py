from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN
import os


br = BuilderAndRunner()
os.environ["MORE"] = "Yes"
br.run(['gprbuild', '-p', '-q', '-Phello_world.gpr'])
br.call([GPRCLEAN])
