from e3.env import Env
from e3.fs import cp
from e3.os.process import Run

if Env().host.platform.endswith('windows'):
    cp('config-windows.cgpr', 'config.cgpr')
    cp('prj_driver-windows.gpr', 'prj_driver.gpr')

p = Run(['gprbuild', '-p', '-q', 'prj.gpr'])

# test native without compiler packages
p = Run(['main', '-P', 'prj.gpr'])
print(p.out)

# test cross without compiler packages & no compiler installed
p = Run(['main', '-P', 'prj_arm_eabi.gpr'])
print(p.out)

# test using config's compiler package
p = Run(['main', '--config', 'config.cgpr', '-P', 'prj.gpr'])
print(p.out)

# test using project's compiler package
# under windows check also casing & executable extension support
p = Run(['main', '-P', 'prj_driver.gpr'])
print(p.out)

# test different compiler defined in project & config
p = Run(['main', '--config', 'config.cgpr', '-P', 'prj_driver.gpr'])
print(p.out)
