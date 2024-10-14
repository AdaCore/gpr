from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN, GPRBUILD
import os


br = BuilderAndRunner()
os.environ["MORE"] = "Yes"
br.run([GPRBUILD, '-p', '-q', '-Phello_world.gpr'])
br.call([GPRCLEAN])
