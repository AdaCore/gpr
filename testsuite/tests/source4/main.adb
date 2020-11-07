------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Unit;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Source.Set;
with GPR2.Project.View;
with GPR2.Project.Tree;
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

      procedure List_Sources (View : Project.View.Object);

      procedure Copy_Source (Name : String);
      --  Copy source file from srcp directory to src

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
                 ("   Kind: "
                  & GPR2.Unit.Library_Unit_Type'Image (S.Kind));

               if U /= "" then
                  Text_IO.Set_Col (60);
                  Text_IO.Put ("unit: " & String (U));
               end if;

               Text_IO.New_Line;
            end;
         end loop;
      end List_Sources;

      -----------------
      -- Copy_Source --
      -----------------

      procedure Copy_Source (Name : String) is
      begin
         Ada.Directories.Copy_File ("srcp/" & Name, "src/" & Name);
      end Copy_Source;

      Prj  : Project.Tree.Object;
      Ctx  : Context.Object;
      View : Project.View.Object;

   begin
      --  Create api-call.adb as a separate

      Project.Tree.Load (Prj, Create (Project_Name), Ctx);

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));

      List_Sources (View);

      delay 2.0;

      Copy_Source ("api.ads");
      Copy_Source ("api.adb");
      Copy_Source ("api-call.adb");

      List_Sources (View);
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "source4");
   begin
      Text_IO.Put (" > " & Filename (I + 8 .. Filename'Last));
   end Output_Filename;

begin
   Check ("demo.gpr");
end Main;
