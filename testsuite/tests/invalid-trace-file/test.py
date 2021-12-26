from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCONFIG


BuilderAndRunner().call([GPRCONFIG, '--batch', '--config=ada'])
