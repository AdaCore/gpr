import os
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL, GPRLS


bnr = BuilderAndRunner()
OK = True

bnr.run(['gprbuild', '-p', '-q', '-aPmylib', '-Pmain/main.gpr'],
        output='run.out')

for line in open("run.out"):
    OK = False
    print('1:' + line)

bnr.call([GPRLS, '-U', '-aPmylib', '-Pmain/main.gpr', '--debugF',
          'helper.o', 'main.o', 'asm.o'])
print('----------')
bnr.call([GPRLS, '-U', '-aPmylib', '-Pmain/main.gpr', '--debugF'])
print('----------')
bnr.call([GPRLS, '-aPmylib', '-Pmain/main.gpr', '--debugF'])

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
for line in open("run.out"):
    OK = False
    print('4:' + line)

print('----------')
bnr.call([GPRLS, '-U', '-aPinstall/share/gpr', '-Pmain/main.gpr', '-d', '--debugF',
          'helper.o', 'asm.o'])

print('----------')
bnr.run([GPRLS, '-v', '-U', '-aPinstall/share/gpr', '-Pmain/main.gpr', '-d',
         '--debugF'],
        output='run.out')

outp = False
start_line = os.path.join("main", "obj", "asm.o")
for line in open("run.out"):
    if line.find(start_line) > 0:
        outp = True
    if outp:
        print(line[:-1])

if OK:
    print('OK')
else:
    print('NOK')
