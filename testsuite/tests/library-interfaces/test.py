import os

from e3.os.process import Run

OK = True

# build 'p' project
Run(['gprbuild', '-p', '-q', '-aPmylib', '-Pmain/main.gpr'], output='run.out')

for line in open("run.out"):
    OK = False
    print('1:' + line)

Run(['gprbuild', '-p', '-q', '-Pmylib/mylib.gpr'], output='run.out')

for line in open("run.out"):
    OK = False
    print('2:' + line)

Run(['gpr2install', '-p', '-q', '-Pmylib/mylib.gpr',
     '--prefix=' + os.path.join(os.getcwd(), 'install')], output='run.out')

for line in open("run.out"):
    OK = False
    print('3:' + line)

Run(['gprbuild', '-p', '-q',
     '-aPinstall/share/gpr', '-Pmain/main.gpr'], output='run.out')

for line in open("run.out"):
    OK = False
    print('4:' + line)

if OK:
    print('OK')
else:
    print('NOK')
