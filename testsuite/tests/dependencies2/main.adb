------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2018, Free Software Foundation, Inc.          --
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
with GPR2.Project.Tree;
with GPR2.Project.View;
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
            Text_IO.New_Line;
            Output_Filename (S.Filename);

            if U /= "" then
               Text_IO.Put ("   unit: " & String (U));
               Text_IO.New_Line;

               Text_IO.Put_Line ("..... DIRECT");

               Prj.Log_Messages.Clear;

               for D of Source.Dependencies loop
                  Text_IO.Put ("  ");
                  Output_Filename (D.Source.Filename);
                  Text_IO.New_Line;
               end loop;

               if Prj.Has_Messages then
                  for M of Prj.Log_Messages.all loop
                     Text_IO.Put_Line (M.Format);
                  end loop;
               end if;

               Text_IO.Put_Line ("..... UNIT");

               for D of
                 Source.Dependencies (Mode => GPR2.Project.Source.Unit)
               loop
                  Text_IO.Put ("  ");
                  Output_Filename (D.Source.Filename);
                  Text_IO.New_Line;
               end loop;

               Text_IO.Put_Line ("..... CLOSURE");

               for D of
                 Source.Dependencies (Mode => GPR2.Project.Source.Closure)
               loop
                  Text_IO.Put ("  ");
                  Output_Filename (D.Source.Filename);
                  Text_IO.New_Line;
               end loop;
            end if;
         end;
      end loop;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "dependencies2");
   begin
      Text_IO.Put (" > " & Filename (I + 13 .. Filename'Last));
   end Output_Filename;

begin
   Check ("demo.gpr");
end Main;
