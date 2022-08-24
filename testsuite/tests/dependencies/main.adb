--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Source.Part_Set;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Source;

with GPR2.Source_Info.Parser.Ada_Language;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Check (Project_Name : Filename_Type);
   --  Do check the given project's sources

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : Filename_Type) is
      Prj  : Project.Tree.Object;
      Ctx  : Context.Object;
      View : Project.View.Object;
   begin
      Project.Tree.Load (Prj, Create (Project_Name), Ctx);

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));

      for Source of View.Sources loop
         declare
            U : constant Optional_Name_Type := Source.Unit_Name (No_Index);
         begin
            Text_IO.New_Line;
            Output_Filename (Source.Path_Name.Value);

            if U /= "" then
               Text_IO.Put ("   unit: " & String (U));
               Text_IO.New_Line;

               Text_IO.Put_Line ("..... DIRECT");

               Prj.Log_Messages.Clear;

               for D of Source.Dependencies loop
                  Text_IO.Put ("  ");
                  Output_Filename (D.Source.Path_Name.Value);
                  Text_IO.New_Line;
               end loop;

               if Prj.Has_Messages then
                  for M of Prj.Log_Messages.all loop
                     Text_IO.Put_Line (M.Format);
                  end loop;
               end if;

               Text_IO.Put_Line ("..... CLOSURE");

               for D of Source.Dependencies (Closure => True) loop
                  Text_IO.Put ("  ");
                  Output_Filename (D.Source.Path_Name.Value);
                  Text_IO.New_Line;
               end loop;

               if Prj.Has_Messages then
                  for M of Prj.Log_Messages.all loop
                     Text_IO.Put_Line (M.Format);
                  end loop;
               end if;
            end if;
         end;
      end loop;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "dependencies");
   begin
      Text_IO.Put (" > " & Filename (I + 13 .. Filename'Last));
   end Output_Filename;

begin
   Check ("demo.gpr");
end Main;
