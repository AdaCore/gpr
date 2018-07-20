------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2018, Free Software Foundation, Inc.            --
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
with GPR2.Path_Name;
with GPR2.Project.Source.Set;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Source;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   use type GPR2.Source.Object;

   procedure Check (Project_Name : Name_Type);
   --  Do check the given project's sources

   function Filter_Filename (Filename : Path_Name.Full_Name) return String;
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
            O : constant GPR2.Source.Object := S.Other_Part;
            U : constant Optional_Name_Type := S.Unit_Name;
         begin
            Text_IO.Put_Line
              (Filter_Filename (S.Path_Name.Value)
               & " -> "
               & (if O = GPR2.Source.Undefined
                  then "undefined"
                  else Filter_Filename (S.Other_Part.Path_Name.Value)));

            Text_IO.Set_Col (4);
            Text_IO.Put ("   language: " & String (S.Language));

            Text_IO.Set_Col (22);
            Text_IO.Put ("   Kind: " & GPR2.Source.Kind_Type'Image (S.Kind));

            if U /= "" then
               Text_IO.Put ("   unit: " & String (U));
            end if;

            Text_IO.Put_Line
              ("   other part : "
               & (if Source.Has_Other_Part
                  then Filter_Filename
                         (Source.Other_Part.Source.Path_Name.Value)
                  else "undefined"));
            Text_IO.New_Line;
         end;
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
