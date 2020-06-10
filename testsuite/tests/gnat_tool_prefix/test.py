from e3.env import Env
from e3.fs import cp
from e3.os.process import Run

if Env().host.platform.endswith('windows'):
    cp('config-windows.cgpr', 'config.cgpr')
    cp('prj_driver-windows.gpr', 'prj_driver.gpr')

Run(['gprbuild', '-p', '-q', 'prj.gpr'], output=None)

# test native without compiler packages
Run(['./main', '-P', 'prj.gpr'], output=None)

# test cross without compiler packages & no compiler installed
Run(['./main', '-P', 'prj_arm_eabi.gpr'], output=None)

# test using config's compiler package
Run(['./main', '--config', 'config.cgpr', '-P', 'prj.gpr'], output=None)

# test using project's compiler package
# under windows check also casing & executable extension support
Run(['./main', '-P', 'prj_driver.gpr'], output=None)

# test different compiler defined in project & config
Run(['./main', '--config', 'config.cgpr', '-P', 'prj_driver.gpr'], output=None)
