from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS

BuilderAndRunner().call([GPRLS, "-Pfiles/not_allowed_empty_attributes.gpr"])
BuilderAndRunner().call([GPRLS, "-Pfiles/allowed_empty_attributes.gpr"])
BuilderAndRunner().call([GPRLS, "-Pfiles/empty_imported.gpr"])
BuilderAndRunner().call([GPRLS, "-Pfiles/empty_extended.gpr"])
BuilderAndRunner().call([GPRLS, "-Pfiles/empty_aggregated.gpr"])
BuilderAndRunner().call([GPRLS, "-Pfiles/space_imported.gpr"])
BuilderAndRunner().call([GPRLS, "-Pfiles/space_extended.gpr"])
BuilderAndRunner().call([GPRLS, "-Pfiles/space_aggregated.gpr"])
