------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--          Copyright (C) 2016-2017, Free Software Foundation, Inc.         --
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

with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.Project.Source.Set;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Source;

procedure Main is

   use Ada;
   use GPR2;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   procedure Output_Filename (Filename : Full_Path_Name);
   --  Remove the leading tmp directory

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      for Source of Prj.Sources loop
         declare
            S : constant GPR2.Source.Object := Source.Source;
            U : constant Optional_Name_Type := S.Unit_Name;
         begin
            Output_Filename (S.Filename);

            Text_IO.Set_Col (16);
            Text_IO.Put ("   language: " & String (S.Language));

            Text_IO.Set_Col (33);
            Text_IO.Put ("   Kind: " & GPR2.Source.Kind_Type'Image (S.Kind));

            if U /= "" then
               Text_IO.Put ("   unit: " & String (U));
            end if;

            Text_IO.New_Line;
         end;
      end loop;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Full_Path_Name) is
      I : constant Positive :=
            Strings.Fixed.Index (Filename, "extended-replace");
   begin
      Text_IO.Put (" > " & Filename (I + 16 .. Filename'Last));
   end Output_Filename;

   Prj1, Prj2 : Project.Tree.Object;
   Ctx        : Context.Object;
begin
   Project.Tree.Load (Prj1, Project.Create ("prj1.gpr"), Ctx);
   Project.Tree.Load (Prj2, Project.Create ("prj2.gpr"), Ctx);

   Text_IO.Put_Line ("**************** Iterator Prj1");

   for P of Prj1 loop
      Display (P, Full => False);
   end loop;

   Text_IO.Put_Line ("**************** Iterator Prj2");

   for P of Prj2 loop
      Display (P, Full => False);
   end loop;
end Main;
