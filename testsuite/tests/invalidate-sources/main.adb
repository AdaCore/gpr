--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Build.Source.Sets;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
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

      procedure List_Sources (View : Project.View.Object);

      ------------------
      -- List_Sources --
      ------------------

      procedure List_Sources (View : Project.View.Object)
      is
         use type GPR2.Build.Unit_Kind;
      begin
         Text_IO.Put_Line ("----------");

         for Source of View.Sources loop
            declare
               U : constant Optional_Name_Type := Source.Unit.Name;
            begin
               Output_Filename (Source.Path_Name.Value);

               Text_IO.Set_Col (20);
               Text_IO.Put ("   language: " & Image (Source.Language));

               Text_IO.Set_Col (36);
               Text_IO.Put
                 ("   Kind: " & Source.Kind'Image);

               if U /= "" then
                  Text_IO.Set_Col (60);
                  Text_IO.Put ("unit: " & String (U));
                  if Source.Kind = Build.S_Separate then
                     Text_IO.Put (".");
                     Text_IO.Put (String (Source.Unit.Separate_Name));
                  end if;
               end if;

               Text_IO.New_Line;
            end;
         end loop;
      end List_Sources;

      Prj  : Project.Tree.Object;
      Ctx  : Context.Object;
      View : Project.View.Object;
      Log  : GPR2.Log.Object;

   begin
      --  Create api-call.adb as a separate

      declare
         File : Text_IO.File_Type;
      begin
         Text_IO.Create (File, Text_IO.Out_File, "src/api-call.adb");
         Text_IO.Put_Line (File, "separate (Api)");
         Text_IO.Put_Line (File, "procedure Call is");
         Text_IO.Put_Line (File, "begin");
         Text_IO.Put_Line (File, "   null;");
         Text_IO.Put_Line (File, "end Call;");
         Text_IO.Close (File);
      end;

      Project.Tree.Load (Prj, Create (Project_Name), Ctx);
      Prj.Update_Sources (Messages => Log);
      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));

      List_Sources (View);

      --  Change api-call.adb to be a child package We need a small
      --  delay to ensure that the timestamp is updated. To be safe we
      --  wait for a full second.

      declare
         File : Text_IO.File_Type;
      begin
         --  Two new packages

         Text_IO.Create (File, Text_IO.Out_File, "src/newapi.ads");
         Text_IO.Put_Line (File, "package NewAPI is");
         Text_IO.Put_Line (File, "end NewAPI;");
         Text_IO.Close (File);

         Text_IO.Create (File, Text_IO.Out_File, "src/newapi.adb");
         Text_IO.Put_Line (File, "package body NewAPI is");
         Text_IO.Put_Line (File, "end NewAPI;");
         Text_IO.Close (File);

         --  And remove the one created above

         Directories.Delete_File ("src/api-call.adb");
      end;

      Prj.Update_Sources (Messages => Log);
      List_Sources (View);
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "sources");
   begin
      Text_IO.Put (" > " & Filename (I + 8 .. Filename'Last));
   end Output_Filename;

begin
   Check ("demo.gpr");
end Main;
