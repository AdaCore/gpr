from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN
import os

br = BuilderAndRunner()
br.call([GPRCLEAN, "-Pprj.gpr", "--RTS=dir/custom_rts"])
os.environ['GPR_RUNTIME_PATH'] = os.path.join(os.getcwd(), 'dir2')
br.call([GPRCLEAN, "-Pprj.gpr", "--RTS=dir3/custom_rts"])
