from gprbuild_utils import gprname

try:
    gprname('-P A/p.gpr -d src *')


except Exception as E:
    print('*** Error: %s' % str(E))
