from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN, GPRLS


ROOTDIR_Def = "-XROOTDIR=build"
proj = "hello_world.gpr"
br = BuilderAndRunner()
br.build(proj, [ROOTDIR_Def, "-p"])
br.call([GPRLS, ROOTDIR_Def, "-U", "-P", proj])
br.call([GPRCLEAN, ROOTDIR_Def])
