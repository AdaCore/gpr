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

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Log;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Source;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Check (Project_Name : Name_Type);
   --  Do check the given project's sources

   procedure Output_Filename (Filename : Full_Path_Name);
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : Name_Type) is
      Prj  : Project.Tree.Object;
      Ctx  : Context.Object;
      View : Project.View.Object;
   begin
      Project.Tree.Load (Prj, Create (Project_Name), Ctx);

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));

      for Source of View.Sources loop
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

   exception
      when E : GPR2.Project_Error =>
         Text_IO.Put_Line
           ("BEFORE: Has unread Message: "
            & Prj.Log_Messages.Has_Element (Read => False)'Img);

         for M of Prj.Log_Messages.all loop
            declare
               F : constant String := M.Sloc.Filename;
               I : constant Natural := Strings.Fixed.Index
                     (F, "/source-errors");
            begin
               Text_IO.Put_Line ("> " & F (I .. F'Last));
               Text_IO.Put_Line (M.Level'Img);
               Text_IO.Put_Line (M.Sloc.Line'Img);
               Text_IO.Put_Line (M.Sloc.Column'Img);
               Text_IO.Put_Line (M.Message);
            end;
         end loop;

         Text_IO.Put_Line
           ("AFTER: Has unread Message: "
            & Prj.Log_Messages.Has_Element (Read => False)'Img);
         Text_IO.New_Line;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Full_Path_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "source-errors");
   begin
      Text_IO.Put (" > " & Filename (I + 14 .. Filename'Last));
   end Output_Filename;

begin
   Check ("demo1.gpr");
   Check ("demo2.gpr");
   Check ("demo3.gpr");
end Main;
