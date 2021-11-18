from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCONFIG, GPRCLEAN


def filter(file):
    with open(file) as F:
        for line in F:
            if 'warning: environment variable' not in line:
                print(line.rstrip())


BuilderAndRunner().run([GPRCONFIG, '--batch', '--db', 'kb_add', '--config=ada'],
                       output='out.txt')
filter('out.txt')
BuilderAndRunner().run([GPRCONFIG, '--db', 'kb_add', '--config=ada'], output='out2.txt')
filter('out2.txt')
BuilderAndRunner().run([GPRCLEAN, '--db', 'kb_add', '-Pprj.gpr'], output='out3.txt')
filter('out3.txt')
