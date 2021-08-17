from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN


br = BuilderAndRunner()
br.build("hello_world.gpr")
br.call([GPRCLEAN])
