import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME, GPRLS, \
                                                 GPRINSTALL, GPRCLEAN
from e3.diff import diff

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# O528-047
try:
    os.chdir('O528-047')
    run([GPRNAME, '-Pprj.gpr', '*.ada'])
    subprocess.run("gprbuild -p -q -P prj.gpr main.2.ada -o main", shell=True)
    subprocess.run("./main")
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# O711-003
try:
    os.chdir('O711-003')
    run([GPRNAME, '-Pprj.gpr', '*.ada', '-f', '*.c', '-f:c', '*.clang'])
    subprocess.run("gprbuild -p -q -P prj.gpr main.ada", shell=True)
    subprocess.run("./main")
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# OA16-060
try:
    os.chdir('OA16-060')
    run([GPRNAME, '-Pprj.gpr', '*.ad?'])
    subprocess.run("gprbuild -p -q -P prj.gpr", shell=True)
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# PC08-062
try:
    os.chdir('PC08-062')
    run([GPRNAME, '-Pprj.gpr', '--ignore-predefined-units', '*.ada'])
    subprocess.run("gprbuild -p -q -P prj.gpr", shell=True)
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# PC08-064
try:
    os.chdir('PC08-064')
    run([GPRNAME, '-Pprj.gpr', '-dsrc1', '-dsrc2', '*t.ada'])
    run([GPRNAME, '-Pprj.gpr', '-dsrc1', '-dsrc2', '*2.ada'])
    run([GPRNAME, '-Pprj.gpr', '-dsrc1', '-dsrc2', '--ignore-duplicate-files', '*.ada'])
    subprocess.run("gprbuild -p -q -P prj.gpr", shell=True)
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# R404-033
try:
    os.chdir('R404-033')
    run([GPRNAME, '-Pp.gpr', '*.adb'])
    print(diff('expected2', 'p.gpr'))
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# RB16-006
try:
    os.chdir('RB16-006')
    if "GPRTOOL" in os.environ:
        os.environ.pop("GPR_TOOL")
    subprocess.run('gprbuild -p -q -P test.gpr', shell=True)
    run([GPRINSTALL, '-p', '-Ptest.gpr', '--prefix=./install'])
    run([GPRLS, '-Ptest.gpr'])
    run([GPRCLEAN, '-Ptest.gpr'])
    run([GPRNAME, '-Ptest.gpr', '-d', 'src', '*'])
except Exception as E:
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# RB29-052
try:
    os.chdir('RB29-052')
    run([GPRNAME, '-PA/p.gpr', "-d", "src", '*'])
except Exception as E:
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# S117-048
try:
    os.chdir('S117-048')
    run([GPRNAME, '-Pp.gpr', '-d', 'src', '*.ads', '-f*.c'])
    print(diff('naming_expected2', 'p_naming.gpr'))
    print(diff('source_list_expected', 'p_source_list.txt'))
except Exception as E:
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')
