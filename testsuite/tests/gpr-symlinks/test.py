import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN, GPRINSTALL

bnr = BuilderAndRunner()

bnr.run(["gprbuild", "-eL", "project/prj.gpr"],
        env={"GPR_PROJECT_PATH": os.getcwd()})

bnr.run([GPRINSTALL, "-p", "--prefix=" + os.getcwd() + "/inst1",
         "-r", "project/prj.gpr"],
        env={"GPR_PROJECT_PATH": os.getcwd()})

bnr.run([GPRINSTALL, "-p", "--prefix=" + os.getcwd() + "/inst2",
         "-r", "-eL", "project/prj.gpr"],
        env={"GPR_PROJECT_PATH": os.getcwd()})

if os.path.exists("inst1"):
    print("NOK inst1 should not have been created")
else:
    print("OK inst1 not created")

if os.path.exists("inst2"):
    print("OK inst2 created")
else:
    print("NOK inst2 should have been created")

if os.path.exists("inst2/include/imported_prj/foo.ads"):
    print("OK foo.ads installed")
else:
    print("NOK foo.ads should have been installed")

if os.path.exists("inst2/lib/imported_prj/foo.ali"):
    print("OK foo.ali installed")
else:
    print("NOK foo.ali should have been installed")

bnr.run([GPRCLEAN, "-r", "-eL", "project/prj.gpr"],
        env={"GPR_PROJECT_PATH": os.getcwd()})

if os.path.exists("import/obj/foo.o"):
    print("NOK foo.o should have been removed")
else:
    print("OK foo.o removed")

if os.path.exists("import/src/foo.ads"):
    print("OK foo.ads found")
