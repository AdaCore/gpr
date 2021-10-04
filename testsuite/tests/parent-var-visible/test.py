from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN, GPRLS


ROOTDIR_Def = "-XROOTDIR=build"
proj = "hello_world.gpr"
br = BuilderAndRunner()
br.run(['gprbuild', '-p', '-q', '-P', proj, ROOTDIR_Def])
br.call([GPRLS, ROOTDIR_Def, "-U", "-P", proj])
br.call([GPRCLEAN, ROOTDIR_Def])
