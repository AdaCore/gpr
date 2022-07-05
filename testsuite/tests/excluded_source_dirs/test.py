import json
from testsuite_support.builder_and_runner import (
    BuilderAndRunner, GPRLS, GPRINSPECT)


bnr = BuilderAndRunner()

print("* non-existing directory value")
bnr.call([GPRLS, "-Pbad.gpr", "-s", "--source-parser"])
print("* excluded dirs from a recursive search")
bnr.call([GPRLS, "-Pprj.gpr", "-s", "--source-parser"])
print("* excluded explicitly defined dirs")
bnr.call([GPRLS, "-Pprj2.gpr", "-s", "--source-parser"])
print("* excluded parent with visible subdir")
bnr.call([GPRLS, "-Pprj3.gpr", "-s", "--source-parser"])
print("* source search path reported by gprinspect")
p = bnr.run([GPRINSPECT, "-Pprj2.gpr", "--display=json"])

if p.status != 0:
    print("could not run gprinspect")
    exit(1)

src_dirs = json.loads(p.out)["tree"]["source-search-paths"]
for src in src_dirs:
    if 'adainclude' in src:
        print("<...lib/gcc/(host)/(version)/adainclude>")
    else:
        print(src)
