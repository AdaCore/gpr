import json
import os.path
from e3.env import Env
from e3.os.process import Run


from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()
test_number = 1


def test(header):
    global test_number
    print("================================================================")
    print("Case " + str(test_number) + " - " + header)
    proc = bnr.call(["gpr2build", "-P", os.path.join("tree", "main.gpr"), "-p"])

    if proc.status:
        print("Test return value: " + str(proc.status))
    else:
        print("== Content of " + os.path.join("tree", "obj", "jobs.json") + ":")
        json_file = open(os.path.join("tree", "obj", "jobs.json"))
        jobs = json.load(json_file)
        error = False

        for job in jobs:
            print(
                "uid: '"
                + job["uid"]
                + "', status : '"
                + job["status"]
                + "', stdout: '"
                + job["stdout"]
                + "', stderr: '"
                + job["stderr"]
                + "'"
            )
            if job["status"] != "SKIPPED" and job["status"] != "0":
                error = True

        if error:
            print("Error detected in jobs.json")
        else:
            print("== Executable output:")
            Run([os.path.join("tree", "obj", "main")])

    print("")
    test_number += 1


def testsuite(file_path):
    with open(file_path, "rb") as file:
        file_content = file.read()

    os.remove(file_path)
    test("Missing " + os.path.basename(file_path))

    if not os.path.exists(file_path):
        with open(file_path, "wb") as file:
            file.write(file_content)

    # Restore state of the project after a correct compilation
    Run(["gpr2build", "-P", os.path.join("tree", "main.gpr"), "-p"])

    with open(file_path, "a") as file:
        file.write("--  Comment that will not prevent a compilation for Ada files")

    test("Modified " + os.path.basename(file_path) + " (comments only)")

    # Restore state of the project after a correct compilation
    Run(["gpr2build", "-P", os.path.join("tree", "main.gpr"), "-p"])


test("Build from scratch")

if "windows" in Env().host.platform:
    testsuite(os.path.join("tree", "obj", "main.exe"))
else:
    testsuite(os.path.join("tree", "obj", "main"))

testsuite(os.path.join("tree", "src", "main.adb"))
testsuite(os.path.join("tree", "src", "pkg.adb"))
testsuite(os.path.join("tree", "src", "pkg.ads"))
testsuite(os.path.join("tree", "src", "dep_two.adb"))
testsuite(os.path.join("tree", "src", "dep_two.ads"))
testsuite(os.path.join("tree", "obj", "b__main.ads"))
testsuite(os.path.join("tree", "obj", "b__main.adb"))
# Now, add content to source files that changes the code execution
file_path = os.path.join("tree", "src", "main.adb")

with open(file_path, "w") as file:
    file.write(
        """with Pkg;
function Main return Integer is
begin
   Pkg.P;
   return 1;
end Main;
"""
    )

test("Modified main.adb with different return code")

file_path = os.path.join("tree", "src", "pkg.adb")
with open(file_path, "r") as file:
    file_save = file.read()

with open(file_path, "w") as file:
    file.write(
        """
with Ada.Text_IO;

package body Pkg is

   procedure P is
   begin
      Ada.Text_IO.Put_Line ("Hello from Pkg");
   end P;
end Pkg;"""
    )

test("Modified pkg.adb without dep_two dependency")

# Restore state of the project after a correct compilation
with open(file_path, "w") as file:
    file.write(file_save)
Run(["gpr2build", "-P", os.path.join("tree", "main.gpr"), "-p"])

file_path = os.path.join("tree", "src", "dep_two.adb")
with open(file_path, "r") as file:
    file_save = file.read()

with open(file_path, "w") as file:
    file.write(
        """
with Ada.Text_IO;

package body Dep_Two is

   procedure P is
   begin
      Ada.Text_IO.Put_Line ("Hello from modified Dep_Two");
   end P;
end Dep_Two;"""
    )

test("Modified dep_two.adb (new output)")

# Restore state of the project after a correct compilation
with open(file_path, "w") as file:
    file.write(file_save)
Run(["gpr2build", "-P", os.path.join("tree", "main.gpr"), "-p"])
