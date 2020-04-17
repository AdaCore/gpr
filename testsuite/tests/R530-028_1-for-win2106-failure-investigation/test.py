import subprocess

from gnatpython.fileutils import ls


try:
    artifacts = ['obj/*', 'lib/obj/*', 'lib/lib/*',
                 'lib/pkg/obj/*', 'lib/pkg/lib/*']

    for args in [
        'gprbuild -v -p -P main.gpr -dn',
        'ls -laR --full-time',
        'gpr2clean -v -p -P main.gpr -r',
        'ls -laR --full-time',
    ]:
        print(subprocess.check_output(args, shell=True))

    if ls(artifacts) == []:
        print("clean OK")
    else:
        print("clean not OK")
        print(ls(artifacts))

except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
