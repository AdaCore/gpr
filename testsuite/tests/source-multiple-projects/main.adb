--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Unit;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Build.Source.Sets;
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
      Log  : GPR2.Log.Object;
   begin
      Project.Tree.Load (Prj, Create (Project_Name), Ctx);

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));

      Prj.Update_Sources (Messages => Log);
      Log.Output_Messages;
   exception
      when GPR2.Project_Error =>
         declare
            --  Use an ordered set to ensure stable output as reading
            --  files on system is not stable across OS.
            package Strings is
              new Containers.Indefinite_Ordered_Sets (String);
            M : Strings.Set;
         begin
            for C in Prj.Log_Messages.Iterate (Information => False) loop
               M.Insert (String'(GPR2.Log.Element (C).Format));
            end loop;
            for I of M loop
               Text_IO.Put_Line (I);
            end loop;
         end;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      D : constant String := "source-multiple-projects";
      S : constant String := String (Filename);
      I : constant Positive := Strings.Fixed.Index (S, D);
   begin
      Text_IO.Put (" > " & S (I + D'Length + 1 .. S'Last));
   end Output_Filename;

begin
   Check ("main.gpr");
   Check ("mainc.gpr");
   Check ("maine.gpr");
   Check ("maina.gpr");
end Main;
