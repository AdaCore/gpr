with Ada.Text_IO;

with GNAT.OS_Lib;

with GPR2.Environment;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use GPR2;

   Sep  : constant String := (1 => GNAT.OS_Lib.Path_Separator);

   Tree : Project.Tree.Object;
   Opt  : Options.Object;
   Env  : GPR2.Environment.Object := GPR2.Environment.Process_Environment;

begin
   --  "default-install" stands for an installation directory. It holds
   --  Installed next to a homonym of C. The C to select is the one from
   --  "lib_c", as it comes first on the project path.
   --
   --  Note that "default-install" is named so that it sorts before the other
   --  directories: imports are loaded in path order, so this is what makes
   --  Installed the first project loaded after P.

   Env.Insert ("GPR_PROJECT_PATH_FILE", "");
   Env.Insert ("ADA_PROJECT_PATH", "");
   Env.Insert
     ("GPR_PROJECT_PATH",
      "deps" & Sep & "lib_c" & Sep & "default-install");

   Opt.Add_Switch (Options.P, "p.gpr");

   if Tree.Load (Opt, Absent_Dir_Error => No_Error, Environment => Env) then
      Ada.Text_IO.Put_Line
        ("dep withs "
         & Tree.Root_Project.View_For ("dep").View_For ("c")
             .Path_Name.String_Value);
   end if;
end Main;
