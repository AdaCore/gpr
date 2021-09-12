import os

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL, GPRLS

bnr = BuilderAndRunner()
OK = True

# build 'p' project
bnr.run(['gprbuild', '-p', '-q', '-aPmylib', '-Pmain/main.gpr'],
        output='run.out')

for line in open("run.out"):
    OK = False
    print('1:' + line)

bnr.call([GPRLS, '-U', '-aPmylib', '-Pmain/main.gpr', '--debugF', 'helper.o', 'main.o'])

bnr.run(['gprbuild', '-p', '-q', '-Pmylib/mylib.gpr'], output='run.out')

for line in open("run.out"):
    OK = False
    print('2:' + line)

bnr.run([GPRINSTALL, '-p', '-q', '-Pmylib/mylib.gpr',
         '--prefix=' + os.path.join(os.getcwd(), 'install')], output='run.out')

for line in open("run.out"):
    OK = False
    print('3:' + line)

bnr.run(['gprbuild', '-p', '-q',
         '-aPinstall/share/gpr', '-Pmain/main.gpr'], output='run.out')
print('----------')
bnr.call([GPRLS, '-U', '-aPinstall/share/gpr', '-Pmain/main.gpr', '-d', '--debugF',
          'helper.o'])

for line in open("run.out"):
    OK = False
    print('4:' + line)

if OK:
    print('OK')
else:
    print('NOK')
