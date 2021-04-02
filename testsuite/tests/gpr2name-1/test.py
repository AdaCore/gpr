import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME, GPRLS, \
                                                 GPRINSTALL, GPRCLEAN
from e3.diff import diff

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# gpr2name -P switch support
try:
    os.chdir('P-switch-support')
    run([GPRNAME, '-Pprj.gpr', '*.ada'])
    print(diff('prj.expected', 'prj.gpr'))
    subprocess.run("gprbuild -p -q -P prj.gpr main.2.ada -o main", shell=True)
    subprocess.run("./main")
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# non ada sources support
try:
    os.chdir('non-ada-sources')
    run([GPRNAME, '-Pprj.gpr', '*.ada', '-f', '*.c', '-f:c', '*.clang'])
    subprocess.run("gprbuild -p -q -P prj.gpr main.ada", shell=True)
    subprocess.run("./main")
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# very long source file support
try:
    os.chdir('very-long-source-file')
    run([GPRNAME, '-Pprj.gpr', '*.ad?'])
    subprocess.run("gprbuild -p -q -P prj.gpr", shell=True)
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# ignore-predefined-units switch support
try:
    os.chdir('ignore-predefined-units')
    run([GPRNAME, '-Pprj.gpr', '--ignore-predefined-units', '*.ada'])
    subprocess.run("gprbuild -p -q -P prj.gpr", shell=True)
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# ignore-duplicate-files switch support
try:
    os.chdir('ignore-duplicate-files')
    run([GPRNAME, '-Pprj.gpr', '-dsrc1', '-dsrc2', '*t.ada'])
    run([GPRNAME, '-Pprj.gpr', '-dsrc1', '-dsrc2', '*2.ada'])
    run([GPRNAME, '-Pprj.gpr', '-dsrc1', '-dsrc2', '--ignore-duplicate-files', '*.ada'])
    subprocess.run("gprbuild -p -q -P prj.gpr", shell=True)
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# keep unknown attributes in generated GPR
try:
    os.chdir('keep-unknown-attributes')
    run([GPRNAME, '-Pp.gpr', '*.adb'])
    print(diff('expected2', 'p.gpr'))
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# set GPR_TOOL to gprbuild when gpr tools are parsing GPR files
try:
    os.chdir('external-GPR_TOOL')
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

# allow -P with path
try:
    os.chdir('project-file-with-path')
    run([GPRNAME, '-PA/p.gpr', "-d", "src", '*'])
except Exception as E:
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# limit generated gpr changes
try:
    os.chdir('order-generated-content')
    run([GPRNAME, '-Pp.gpr', '-d', 'src', '*.ads', '-f*.c'])
    print(diff('naming_expected2', 'p_naming.gpr'))
    print(diff('source_list_expected', 'p_source_list.txt'))
except Exception as E:
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# source_files attributes should be removed
try:
    os.chdir('remove-source-files-attr')
    run([GPRNAME, '-Pprj.gpr', '*.ada'])
    subprocess.run("gprbuild -q -P prj.gpr", shell=True)
except Exception as E:
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')

# invalid gpr handling test
try:
    p = BuilderAndRunner().run([GPRNAME, '-Pinvalid-gpr-file/prj', '*.body'])
    if p.status != 0:
        print('gprname returned ' + str(p.status))
        print(p.out)
except Exception as E:
    print('*** Error: %s' % str(E))
