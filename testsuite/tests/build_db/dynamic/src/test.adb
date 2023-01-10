with Ada.Text_IO;

with GPR2.Build.Tree_Db;
with GPR2.Build.View_Db;
with GPR2.Build.Source_Info.Sets;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Objects; use Objects;

package body Test is

   use GPR2;
   use GPR2.Build;

   ----------
   -- Dump --
   ----------

   procedure Dump is

      ---------------------
      -- Print_Unit_Part --
      ---------------------

      procedure Print_Unit_Part
        (Kind     : Unit_Kind;
         View     : Project.View.Object;
         Path     : Path_Name.Object;
         Index    : Unit_Index;
         Sep_Name : Optional_Name_Type)
      is
         Root : constant Path_Name.Object := Tree.Root_Project.Dir_Name;
      begin
         Ada.Text_IO.Put_Line
           ("     " &
            (if Kind = S_Spec then "spec: "
               elsif Kind = S_Body then "body: "
               else "sep. " & String (Sep_Name) & ": ")
            & String (Path.Relative_Path (Root).Name)
            & (if Index = No_Index then "" else " @" & Index'Image)
            & " (from view " & String (View.Name) & ")");
      end Print_Unit_Part;

      ------------------
      -- Print_Source --
      ------------------

      procedure Print_Source (S : Build.Source_Info.Object) is
         Root : constant Path_Name.Object := Tree.Root_Project.Dir_Name;

         function Image (Kind : Unit_Kind) return String
         is (case Kind is
                when S_Spec     => "spec",
                when S_Body     => "body",
                when S_Separate => "sep.");
      begin
         Ada.Text_IO.Put ("   - " &
                            String (S.Path_Name.Relative_Path (Root).Name));

         if not S.Has_Units or else not S.Has_Index then
            Ada.Text_IO.Put_Line (" (" & Image (S.Kind) & ")");
         else
            Ada.Text_IO.New_Line;
         end if;
      end Print_Source;

   begin
      Ada.Text_IO.Put_Line ("* Sources:");

      for C in Tree.Iterate loop
         declare
            V : constant Project.View.Object := Project.Tree.Element (C);
            Has_Sources : Boolean := False;
         begin
            if V.Kind in GPR2.With_Source_Dirs_Kind then
               Ada.Text_IO.Put (" - sources of " & String (V.Name));

               if V.Is_Extended then
                  Ada.Text_IO.Put (" extended by " &
                                     String (V.Extending.Name));
               end if;

               Ada.Text_IO.New_Line;

               for S of Db.View_Database (V).Sources (Sorted => True) loop
                  Print_Source (S);
                  Has_Sources := True;
               end loop;

               if not Has_Sources then
                  Ada.Text_IO.Put_Line ("   (no sources)");
               end if;
            end if;
         end;
      end loop;

      Ada.Text_IO.Put_Line ("* Compilation units:");

      if Tree.Root_Project.Kind = K_Aggregate then
         for V of Tree.Root_Project.Aggregated loop
            Ada.Text_IO.Put_Line (" - units of subtree " & String (V.Name) & ':');

            for U of Db.View_Database (V).Compilation_Units loop
               Ada.Text_IO.Put_Line ("   - " & String (U.Name));
               U.For_All_Part (Print_Unit_Part'Access);
            end loop;
         end loop;
      else
         Ada.Text_IO.Put_Line (" - units of " & String (Tree.Root_Project.Name));

         for U of Db.View_Database (Tree.Root_Project).Compilation_Units loop
            Ada.Text_IO.Put_Line ("   - " & String (U.Name));
            U.For_All_Part (Print_Unit_Part'Access);
         end loop;
      end if;
   end Dump;

end Test;
