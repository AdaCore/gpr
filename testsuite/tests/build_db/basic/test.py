from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

bnr.build(project='prj.gpr',
          args=["-g1", "-q", "-p", "-bargs", "-Es"],
          env={})

bnr.call(['./main', 'trees/agg/agg.gpr', 'pkg.ads', 'not_found.txt'])
bnr.call(['./main', 'trees/agg2/agg.gpr'])
bnr.call(['./main', 'trees/multi/prj.gpr'])
bnr.call(['./main', 'trees/invalid_naming/prj.gpr'])
bnr.call(['./main', 'trees/separates/lib.gpr'])
bnr.call(['./main', 'trees/separates/prj.gpr'])
