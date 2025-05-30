from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

bnr.build(project='prj.gpr',
          args=["-g1", "-q", "-p", "-bargs", "-Es"],
          env={})

bnr.call(['./main', 'projects/base.gpr'])
bnr.call(['./main', 'projects/ext.gpr'])
bnr.call(['./main', 'projects/extext.gpr'])
