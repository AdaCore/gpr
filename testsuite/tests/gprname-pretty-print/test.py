import os

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# generated project file indentation
try:
    for c in 'cdefghijklmno':
        p = 'subdirectory_' + c
        if not os.path.exists(p):
            os.mkdir(p)
    run([GPRNAME, '-P', 'prj.gpr', '-dsubdirectory_a', '-dsubdirectory_b',
         '-dsubdirectory_c', '-dsubdirectory_d', '-dsubdirectory_e',
         '-dsubdirectory_f', '-dsubdirectory_g', '-dsubdirectory_h',
         '-dsubdirectory_i', '-dsubdirectory_j', '-dsubdirectory_k',
         '-dsubdirectory_l', '-dsubdirectory_m', '-dsubdirectory_n',
         '-dsubdirectory_o', '*.ad?'])
    os.system('cat prj.gpr')
except Exception as E:
    print('*** Error: %s' % str(E))
