--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Unit;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Source.Set;
with GPR2.Project.View;
with GPR2.Project.Tree;

with GPR2.Source_Info.Parser.Ada_Language;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Check (Project_Name : Filename_Type);
   --  Do check the given project's sources

   function Filter_Filename (Filename : Path_Name.Full_Name) return String;
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
         Text_IO.Put_Line
           (Filter_Filename (Source.Path_Name.Value)  & " -> " &
             (if Source.Has_Other_Part (No_Index)
              then Filter_Filename
                (Source.Other_Part (No_Index).Source.Path_Name.Value)
              else "undefined"));

         Text_IO.Set_Col (4);
         Text_IO.Put ("   language: " & Image (Source.Language));

         Text_IO.Set_Col (22);
         Text_IO.Put
           ("   Kind: "
              & GPR2.Unit.Library_Unit_Type'Image (Source.Kind));

         if Source.Has_Units then
            Text_IO.Put ("   unit: " & String (Source.Unit_Name));
         end if;

         Text_IO.New_Line;
      end loop;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   function Filter_Filename (Filename : Path_Name.Full_Name) return String is
      D : constant String := "source-other-part";
      I : constant Positive := Strings.Fixed.Index (Filename, D);
   begin
      return Filename (I + D'Length .. Filename'Last);
   end Filter_Filename;

begin
   Check ("demo.gpr");
end Main;
