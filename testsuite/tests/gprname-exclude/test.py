from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME
import subprocess


try:
    BuilderAndRunner().check_output([GPRNAME, "-P", "test.gpr", "*.ada", "-x",
                                     "*exc?ude*"])
    subprocess.run("gprbuild -p -q -P prj.gpr", shell=True)
    subprocess.run("./main")

except Exception as E:
    print('*** Error: %s' % str(E))
