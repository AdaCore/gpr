import json
from testsuite_support.builder_and_runner import (
    BuilderAndRunner, GPRLS, GPRINSPECT)


bnr = BuilderAndRunner()

print("* non-existing directory value")
bnr.call([GPRLS, "-Pfiles/bad.gpr", "-s", "--source-parser"])
print("* excluded dirs from a recursive search")
bnr.call([GPRLS, "-Pfiles/prj.gpr", "-s", "--source-parser"])
print("* excluded explicitly defined dirs")
bnr.call([GPRLS, "-Pfiles/prj2.gpr", "-s", "--source-parser"])
print("* excluded parent with visible subdir")
bnr.call([GPRLS, "-Pfiles/prj3.gpr", "-s", "--source-parser"])
print("* exclude \"\"")
bnr.call([GPRLS, "-Pexclude_empty_string/prj.gpr", "-s", "--source-parser"])
print("* exclude recursive")
bnr.call([GPRLS, "-Pexclude_recursive/prj.gpr", "-s", "--source-parser"])
print("* exclude \"**\"")
bnr.call([GPRLS, "-Pexclude_star_star/prj.gpr", "-s", "--source-parser"])
print("* exclude absolute path")
bnr.call([GPRLS, "-Pexclude_absolute_path/prj.gpr", "-s", "--source-parser"])
print("* exclude non existing absolute path")
bnr.call([GPRLS, "-Pexclude_absolute_path/bad.gpr", "-s", "--source-parser"])
print("* source search path reported by gprinspect")
p = bnr.run([GPRINSPECT, "-Pfiles/prj2.gpr", "--display=json"])

if p.status != 0:
    print("could not run gprinspect")
    exit(1)

src_dirs = json.loads(p.out)["tree"]["source-search-paths"]
for src in src_dirs:
    if 'adainclude' in src:
        print("<...lib/gcc/(host)/(version)/adainclude>")
    else:
        print(src)
