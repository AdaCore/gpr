#!/usr/bin/env python

import os
import subprocess
from testsuite_support.builder_and_runner import BuilderAndRunner


src_dir = "src"
if not os.path.exists(src_dir):
    os.mkdir(src_dir)

PN = 10

for x in range(1, PN):
    with open("%s/p%d.adb" % (src_dir, x), "w+") as f:
        f.write("with P%d;\n" % (x + 1))
        f.write("procedure P%d is\n" % x)
        f.write("begin\n")
        f.write("   P%d;\n" % (x + 1))
        f.write("end P%d;\n" % x)

    with open("%s/p%d.ads" % (src_dir, x), "w+") as f:
        f.write("procedure P%d;\n" % x)

with open("%s/p%d.ads" % (src_dir, PN), "w+") as f:
    f.write("procedure P%d;\n" % PN)

with open("%s/p%d.adb" % (src_dir, PN), "w+") as f:
    f.write("procedure P%d is\n" % PN)
    f.write("begin\n")
    f.write("   null;\n")
    f.write("end P%d;\n" % PN)


BuilderAndRunner().build("check_mem.gpr", args=['-p'])

EXEC = "obj/check_mem"
first_final = False

#  Run driver 2 times
for r in range(0, 2):
    BuilderAndRunner().call([EXEC, str(r * 2 + 1)])
    with open("run%d.out" % r, "w+") as ofn:
        subprocess.call(["gnatmem", "-t", "0", EXEC], stdout=ofn)
        ofn.seek(0)
        for line in ofn:
            if line[0:8] == "   Final":
                if first_final:
                    if first_final == line:
                        print("OK final water mark")
                    else:
                        print(first_final + line)
                else:
                    first_final = line

            elif line.find("GNATMEM dump file corrupt") >= 0:
                print(line)
