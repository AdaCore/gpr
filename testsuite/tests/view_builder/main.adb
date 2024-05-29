with Ada.Text_IO;
with Ada.Directories;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main
is
   use GPR2;

   procedure Test (Dir : String) is
      Old_CWD : constant String := Ada.Directories.Current_Directory;
      Tree    : Project.Tree.Object;
      Opt     : Options.Object;
   begin
      Ada.Directories.Set_Directory ("demo/src1");

      --  Load implicit project created via the view_builder
      if Tree.Load (Opt) then
         Ada.Text_IO.Put_Line (String (Tree.Root_Project.Name) & " loaded");

         Tree.Update_Sources;

         for S of Tree.Root_Project.Sources loop
            Ada.Text_IO.Put_Line ("  " & String (S.Path_Name.Simple_Name));
         end loop;
      end if;

      Ada.Directories.Set_Directory (Old_CWD);
   end Test;

begin
   Test ("demo/src1");
   Test ("demo/src2");
end Main;
