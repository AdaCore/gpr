--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Unit;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Source_Info.Parser.Ada_Language;

with U3;

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
            D : Path_Name.Object;
         begin
            Output_Filename (Source.Path_Name.Value);

            Text_IO.Set_Col (20);
            Text_IO.Put ("   language: " & Image (Source.Language));

            if Source.Has_Units then
               for Unit of Source.Units loop
                  Text_IO.Set_Col (40);
                  Text_IO.Put ("Kind: "
                                 & GPR2.Unit.Library_Unit_Type'Image (Unit.Kind));
                  Text_IO.Put_Line ("   unit: " & String (Source.Unit_Name (Unit.Index)));
                  if Source.Artifacts.Has_Dependency (Unit.Index) then
                     D := Source.Artifacts.Dependency (Unit.Index);
                     if D.Exists then
                        Text_IO.Set_Col (40);
                        Text_IO.Put_Line
                          ("deps: " & String (D.Simple_Name));
                     end if;
                  end if;
               end loop;
            end if;
         end;
      end loop;

      --  remove the ali files after printing all source infos (so that we don't
      --  remove a multi-unit ali before printing all of its units)
      for Source of View.Sources loop
         if Source.Has_Units then
            for Unit of Source.Units loop
               if Source.Artifacts.Has_Dependency (Unit.Index) then
                  declare
                     D : Path_Name.Object;
                  begin
                     D := Source.Artifacts.Dependency (Unit.Index);
                     if D.Exists then
                        Directories.Delete_File (D.Value);
                     end if;
                  end;
               end if;
            end loop;
         end if;
      end loop;

   exception
      when E : GPR2.Project_Error =>
         Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         for M of Prj.Log_Messages.all loop
            Text_IO.Put_Line (M.Format);
         end loop;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "source-ali");
   begin
      Text_IO.Put (" > " & Filename (I + 8 .. Filename'Last));
   end Output_Filename;

begin
   U3;
   for J in Boolean loop
      Check ("source_ali.gpr");
   end loop;
end Main;
