------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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
with Ada.Text_IO;

package body GPRls.Options is

   use Ada;

   use type Path_Name.Object;

   -----------------------------
   -- Build_From_Command_Line --
   -----------------------------

   procedure Build_From_Command_Line (Self : in out Object) is separate;

   -----------
   -- Print --
   -----------

   procedure Print (Self : Object) is
   begin
      Text_IO.Put_Line ("Files:");
      for F of Self.Files loop
         Text_IO.Put_Line ("   " & F);
      end loop;

      Text_IO.Put_Line ("Project file: " & Self.Project_File.Value);

      Text_IO.Put_Line ("Project search path:");
      for P of Self.Tree.Project_Search_Paths loop
         Text_IO.Put_Line ("   " & P.Value);
      end loop;

      Text_IO.Put_Line ("Target: " & To_String (Self.Target));

      for R in Self.RTS_Map.Iterate loop
         declare
            Lang : constant Name_Type :=
                     GPR2.Containers.Name_Value_Map_Package.Key (R);
         begin
            Text_IO.Put_Line
              ("RTS"
               & (if Lang = "Ada" then "" else '(' & String (Lang) & ')')
               & ": " & Self.RTS_Map (R));
         end;
      end loop;

      if Self.List_File /= Path_Name.Undefined then
         Text_IO.Put_Line ("List file: " & Self.List_File.Value);
      end if;

      Text_IO.Put_Line ("Project context:");
      for Curs in Self.Project_Context.Iterate loop
         declare
            K : constant Name_Type := Context.Key_Value.Key (Curs);
            V : constant Value_Type := Context.Key_Value.Element (Curs);
         begin
            Text_IO.Put_Line ("   " & String (K) & " => " & V);
         end;
      end loop;

      Text_IO.Put_Line ("With predefined units: " &
                          Self.With_Predefined_Units'Img);
      Text_IO.Put_Line ("Print units: " & Self.Print_Units'Img);
      Text_IO.Put_Line ("Print sources: " & Self.Print_Sources'Img);
      Text_IO.Put_Line ("Print object files: " & Self.Print_Object_Files'Img);
      Text_IO.Put_Line ("Dependency mode: " & Self.Dependency_Mode'Img);
      Text_IO.Put_Line ("Closure mode: " & Self.Closure_Mode'Img);
      Text_IO.Put_Line ("All projects: " & Self.All_Projects'Img);

      Text_IO.Put_Line ("Verbosity: " & Self.Verbosity'Img);
      Text_IO.Put_Line ("Parsing verbosity: " & Self.Verbose_Parsing'Img);
   end Print;

end GPRls.Options;
