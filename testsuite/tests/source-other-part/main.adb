--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Source.Sets;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Build;
   use GPR2.Project;

   procedure Check (Project_Name : Filename_Type);
   --  Do check the given project's sources

   function Filter_Filename (Filename : Path_Name.Full_Name) return String;
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : Filename_Type) is
      Prj   : Project.Tree.Object;
      Ctx   : Context.Object;
      View  : Project.View.Object;
      Log   : GPR2.Log.Object;
      Other : GPR2.Path_Name.Object;
   begin
      Project.Tree.Load (Prj, Create (Project_Name), Ctx);
      Prj.Update_Sources (Messages => Log);
      Log.Output_Messages;

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));

      for Source of View.Sources loop
         Other := Path_Name.Undefined;

         if Source.Has_Units then
            declare
               Unit : GPR2.Build.Compilation_Unit.Object :=
                        View.Unit (Source.Unit.Name);
            begin
               if Source.Kind = S_Spec and then Unit.Has_Part (S_Body) then
                  Other := Unit.Get (S_Body).Source;
               elsif Source.Kind = S_Body and then Unit.Has_Part (S_Spec) then
                  Other := Unit.Get (S_Spec).Source;
               end if;
            end;
         end if;

         Text_IO.Put_Line
           (Filter_Filename (Source.Path_Name.Value)  & " -> " &
            (if Other.Is_Defined
               then Filter_Filename (Other.Value)
               else "undefined"));

         Text_IO.Set_Col (4);
         Text_IO.Put ("   language: " & Image (Source.Language));

         Text_IO.Set_Col (22);
         Text_IO.Put
           ("   Kind: " & Source.Kind'Image);

         if Source.Has_Units then
            Text_IO.Put ("   unit: " & String (Source.Unit.Name));
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
