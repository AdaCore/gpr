import os, subprocess

# Test a standard project

output=subprocess.check_output('gprbuild -p -q prj.gpr', shell=True)

output=subprocess.check_output('gpr2install -p  --prefix='
                               + os.getcwd() + '/inst prj.gpr', shell=True)

if os.path.exists('inst/lib/prj/pck.ci'):
    print("OK pck.ci")
else:
    print("NOK pck.ci")

if os.path.exists('inst/lib/prj/pck2.ci'):
    print("OK pck2.ci")
else:
    print("NOK pck2.ci")

output=subprocess.check_output('gpr2install -p  --uninstall --prefix='
                               + os.getcwd() + '/inst prj.gpr', shell=True)

if os.path.exists('inst/lib/prj/pck.ci'):
    print("NOK pck.ci")
else:
    print("OK pck.ci")

if os.path.exists('inst/lib/prj/pck2.ci'):
    print("NOK pck2.ci")
else:
    print("OK pck2.ci")

# Test a library project

output=subprocess.check_output('gprbuild -p -q lib.gpr', shell=True)

output=subprocess.check_output('gpr2install -p  --prefix='
                               + os.getcwd() + '/instl lib.gpr', shell=True)

if os.path.exists('instl/lib/lib/pck.ci'):
    print("OK pck.ci")
else:
    print("NOK pck.ci")

if os.path.exists('instl/lib/lib/pck2.ci'):
    print("OK pck2.ci")
else:
    print("NOK pck2.ci")

output=subprocess.check_output('gpr2install -p  --uninstall --prefix='
                               + os.getcwd() + '/instl lib.gpr', shell=True)

if os.path.exists('instl/lib/lib/pck.ci'):
    print("NOK pck.ci")
else:
    print("OK pck.ci")

if os.path.exists('instl/lib/lib/pck2.ci'):
    print("NOK pck2.ci")
else:
    print("OK pck2.ci")
