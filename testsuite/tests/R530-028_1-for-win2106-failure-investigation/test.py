import subprocess
from gnatpython.fileutils import ls

try:
    artifacts = ['obj/*', 'lib/obj/*', 'lib/lib/*',
                 'lib/pkg/obj/*', 'lib/pkg/lib/*']


    output=subprocess.check_output('gprbuild -p -P main.gpr -dn', shell=True)
    print output
    output=subprocess.check_output('gpr2clean -v -p -P main.gpr -r', shell=True)
    print output

    if ls(artifacts) == []:
        print "clean OK"
    else:
        print "clean not OK"
        print ls(artifacts)

except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
