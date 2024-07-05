from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS

bnr = BuilderAndRunner()

for version in ("5.04a1", "7.2.2", "7.3.2", "wave"):
    print(f"check ALI produced by GNAT {version}:")
    bnr.call([GPRLS, "-P", "prj.gpr", f"-XGNATVersion={version}", "--closure"])
