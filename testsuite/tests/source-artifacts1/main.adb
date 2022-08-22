--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.View;
with GPR2.Project.Tree;

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
            A : constant GPR2.Project.Source.Artifact.Object :=
                  Source.Artifacts;
         begin
            Output_Filename (Source.Path_Name.Value);
            if Source.Has_Units then
               for CU of Source.Units loop
                  if A.Has_Object_Code (CU.Index) then
                     Output_Filename (A.Object_Code (CU.Index).Value);
                  end if;

                  if A.Has_Dependency (CU.Index) then
                     Output_Filename (A.Dependency (CU.Index).Value);
                  end if;
               end loop;

            else
               Output_Filename (A.Object_Code (0).Value);
               Output_Filename (A.Dependency (0).Value);
            end if;

            Text_IO.New_Line;
         end;
      end loop;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive :=
            Strings.Fixed.Index (Filename, "source-artifacts1");
   begin
      Text_IO.Put_Line (" > " & Filename (I + 18 .. Filename'Last));
   end Output_Filename;

begin
   Check ("demo1.gpr");
   Check ("demo2.gpr");
end Main;
