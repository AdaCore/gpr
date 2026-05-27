from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run([GPRBUILD, "-f", "-vh", "-P", "prj.gpr", "-p", "--source-info=sourceinfo.txt"]) # output redirection to "output.txt"
# -vh was quite verbose and still is, the previous test was using a filter on the output
# with Ada.Text_IO; use Ada.Text_IO;
# 
# procedure Filter is
#    Reading : constant String := "Reading source info file";
#    Writing : constant String := "Writing new source info file";
# 
#    File : File_Type;
#    Line : String (1 .. 1_000);
#    Last : Natural;
# 
# begin
#    Open (File, In_File, "output.txt");
# 
#    while not End_Of_File (File) loop
#       Get_Line (File, Line, Last);
# 
#       if Last > Reading'Length and then
#          Line (1 .. Reading'Length) = Reading
#       then
#          Put_Line (Reading);
# 
#       elsif Last > Writing'Length and then
#          Line (1 .. Writing'Length) = Writing
#       then
#          Put_Line (Writing);
#       end if;
#    end loop;
# 
#    Close (File);
# end Filter;
run(["./bin/main"])
run(["ls", "obj/*.txt"])
run([GPRCLEAN, "-r", "-q", "-P", "prj.gpr"])
run([GPRBUILD, "-vh", "-P", "prj.gpr", "--source-info=sourceinfo.txt"]) # output redirection to "output.txt"
# same filtering of the output as shown above
run(["./bin/main"])
