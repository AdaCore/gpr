import subprocess
from utils import filter_cwd

try:
    subprocess.check_output('gpr2clean --config=no.cgpr -P p.gpr',
                            stderr=subprocess.STDOUT,
                            shell=True)
except Exception as E:
    print E.returncode
    print filter_cwd(E.output)

try:
    subprocess.check_output('gpr2install --prefix=p --config=no.cgpr -P p.gpr',
                            stderr=subprocess.STDOUT,
                            shell=True)
except Exception as E:
    print E.returncode
    print filter_cwd(E.output)
