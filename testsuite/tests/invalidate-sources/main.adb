------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--          Copyright (C) 2017-2018, Free Software Foundation, Inc.         --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Source.Set;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Source;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Check (Project_Name : Name_Type);
   --  Do check the given project's sources

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : Name_Type) is

      procedure List_Sources (View : Project.View.Object);

      ------------------
      -- List_Sources --
      ------------------

      procedure List_Sources (View : Project.View.Object) is
      begin
         Text_IO.Put_Line ("----------");

         for Source of View.Sources loop
            declare
               S : constant GPR2.Source.Object := Source.Source;
               U : constant Optional_Name_Type := S.Unit_Name;
            begin
               Output_Filename (S.Path_Name.Value);

               Text_IO.Set_Col (20);
               Text_IO.Put ("   language: " & String (S.Language));

               Text_IO.Set_Col (36);
               Text_IO.Put
                 ("   Kind: " & GPR2.Source.Kind_Type'Image (S.Kind));

               if U /= "" then
                  Text_IO.Set_Col (60);
                  Text_IO.Put ("unit: " & String (U));
               end if;

               Text_IO.New_Line;
            end;
         end loop;
      end List_Sources;

      Prj  : Project.Tree.Object;
      Ctx  : Context.Object;
      View : Project.View.Object;

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

      Prj.Invalidate_Sources (View);

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
