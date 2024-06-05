with GPR2;
with GPR2.Project.Tree;
with GPR2.Options;
with GPR2.Environment;

with Ada.Directories;
with Ada.Text_IO;

with GNAT.OS_Lib;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;
   Env  : GPR2.Environment.Object := GPR2.Environment.Process_Environment;

   use GPR2;

begin
   Env.Insert ("GPR_PROJECT_PATH", "");
   Env.Set_Inherit (True);

   Opt.Add_Switch (Options.P, "preinstalled.gpr");
   Opt.Add_Switch (Options.X, "VSB_DIR=.");
   Opt.Add_Switch (Options.Target, "x86_64-wrs-vxworks7");
   Opt.Add_Switch (Options.RTS, "rtp", "Ada");

   if Tree.Load (Opt, Absent_Dir_Error => No_Error, Environment => Env) then
      Ada.Text_IO.Put_Line ("Cross:");
      Ada.Text_IO.Put_Line (Tree.Root_Project.Path_Name.String_Value);
   end if;
   Tree.Unload;

   --  For the native case we need to manipulate the path a bit to get rid of
   --  actual native compilers.
   Env.Insert
     ("PATH",
      Ada.Directories.Current_Directory
      & GNAT.OS_Lib.Directory_Separator
      & "fake-ada-native"
      & GNAT.OS_Lib.Directory_Separator
      & "bin");

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "preinstalled.gpr");
   Opt.Add_Switch (Options.Target, "all");

   if Tree.Load (Opt, Absent_Dir_Error => No_Error, Environment => Env) then
      Ada.Text_IO.Put_Line ("Native:");
      Ada.Text_IO.Put_Line (Tree.Root_Project.Path_Name.String_Value);
   end if;
   Tree.Unload;
end Main;
