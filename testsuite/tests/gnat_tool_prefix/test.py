from e3.env import Env
from e3.fs import cp
from testsuite_support.builder_and_runner import BuilderAndRunner

if Env().host.platform.endswith('windows'):
    cp('config-windows.cgpr', 'config.cgpr')
    cp('prj_driver-windows.gpr', 'prj_driver.gpr')

bnr = BuilderAndRunner()
bnr.build('prj.gpr')

# test native without compiler packages
bnr.run(['./main', '-P', 'prj.gpr'], output=None)

# test cross without compiler packages & no compiler installed
bnr.run(['./main', '-P', 'prj_arm_eabi.gpr'], output=None)

# test using config's compiler package
bnr.run(['./main', '--config', 'config.cgpr', '-P', 'prj.gpr'], output=None)

# test using project's compiler package
# under windows check also casing & executable extension support
bnr.run(['./main', '-P', 'prj_driver.gpr'], output=None)

# test different compiler defined in project & config
bnr.run(['./main', '--config', 'config.cgpr', '-P', 'prj_driver.gpr'],
        output=None)
