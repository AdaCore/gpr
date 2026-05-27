from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

bnr.build(project='test.gpr', args=["-q", "-p"], env={})

bnr.call(['./main'])
