from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRCONFIG


BuilderAndRunner().call([GPRCONFIG, '--batch', '--config=ada'])
