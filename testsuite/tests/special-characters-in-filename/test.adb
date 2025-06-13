with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Build.Source.Sets; use GPR2.Build.Source.Sets;
with GNATCOLL.VFS; use GNATCOLL.VFS;

function Test return Integer is
   Options : GPR2.Options.Object;
   Tree : GPR2.Project.Tree.Object;

   procedure Create_Backslash_Source is
      Special_File : constant GNATCOLL.VFS.Virtual_File :=
         GNATCOLL.VFS.Create_From_Base ("src\""bar.c", "tree");

      Special_Writable_File : Writable_File := Write_File (Special_File);

   begin
      if Special_Writable_File = Invalid_File then
         Ada.Text_IO.Put_Line ("Failed to create writable file");
         return;
      end if;

      Write (Special_Writable_File, "int" & ASCII.LF &
        "bar (int a, int b, int c)" & ASCII.LF &
        "{" & ASCII.LF &
        "  if (a)" & ASCII.LF &
        "    return b;" & ASCII.LF &
        "  else" & ASCII.LF &
        "    return c;" & ASCII.LF &
        "}" & ASCII.LF &
        "" & ASCII.LF &
        "int foo(int i){" & ASCII.LF &
        "  return i;" & ASCII.LF &
        "}");

      Close (Special_Writable_File);

   end Create_Backslash_Source;

begin
   --  Create the file from source, as adding it directly to the test
   --  directory could be problematic on Windows platforms. This test
   --  is not run on Windows.

   Create_Backslash_Source;
   Options.Add_Switch (GPR2.Options.P, "tree/prj.gpr");

   if not Tree.Load
     (Options, With_Runtime => False, Artifacts_Info_Level => GPR2.Sources_Units)
   then
      raise Program_Error;
   end if;

   Tree.Update_Sources;

   Ada.Text_IO.Put_Line ("sources:");
   for S of Tree.Root_Project.Sources loop
      Ada.Text_IO.Put_Line (String (S.Path_Name.Simple_Name));
   end loop;

   return 0;
end Test;