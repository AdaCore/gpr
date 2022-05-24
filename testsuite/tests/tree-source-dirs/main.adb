with Ada.Text_IO;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is
   use GPR2;

   procedure Test (File : String;
                   View_Path : GPR2.Path_Name.Object);

   procedure Test (File : String;
                   View_Path : GPR2.Path_Name.Object) is
      Project_Tree : Project.Tree.Object;
      Ctx          : constant Context.Object := Context.Empty;

      Src_Dirs : GPR2.Path_Name.Set.Object;

      function "<" (L, R : GPR2.Path_Name.Object) return Boolean
      is (L.Value < R.Value);
      package Sort is new GPR2.Path_Name.Set.Set.Generic_Sorting;

      procedure Check (View : GPR2.Project.View.Object;
                       Externally_Built : Boolean);

      procedure Check (View : GPR2.Project.View.Object;
                       Externally_Built : Boolean) is
      begin
         Ada.Text_IO.Put_Line
           ("Externally_Built = " & Externally_Built'Image);

         if View.Is_Defined then
            Ada.Text_IO.Put_Line ("View:" & String (View.Path_Name.Name));
            Src_Dirs := Project_Tree.Source_Directories
              (View             => View,
               Externally_Built => Externally_Built);
         else
            Ada.Text_IO.Put_Line ("View:Root_Project");
            Src_Dirs := Project_Tree.Source_Directories
              (Externally_Built => Externally_Built);
         end if;
         Sort.Sort (Src_Dirs);
         for Src_Dir of Src_Dirs loop
            Ada.Text_IO.Put_Line (Src_Dir.Value);
         end loop;

      end Check;

      use GPR2.Log;

      View : GPR2.Project.View.Object;

      use GPR2.Path_Name;
   begin
      Ada.Text_IO.Put_Line (File);

      Project_Tree.Load_Autoconf
        (Filename          => Project.Create (GPR2.Filename_Type (File)),
         Context           => Ctx);

      Check (View, False);

      if View_Path.Is_Defined then
         for V of Project_Tree.Ordered_Views loop
            if V.Path_Name.Simple_Name = View_Path.Simple_Name then
               View := V;
               exit;
            end if;
         end loop;
      end if;

      Check (View, True);

      Project_Tree.Unload;

   exception
      when Project_Error =>
         for M of Project_Tree.Log_Messages.all loop
            Ada.Text_IO.Put_Line (M.Format);
         end loop;
   end Test;

begin

   Test ("files/abstract1.gpr", GPR2.Path_Name.Undefined);
   Test ("files/aggregating.gpr",
         GPR2.Path_Name.Create_File ("files/aggregated.gpr"));
   Test ("files/agg_lib.gpr", GPR2.Path_Name.Undefined);

end Main;
