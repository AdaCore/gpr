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

with Ada.Strings.Unbounded;

with GPR2.Project.Registry.Attribute;

package body GPR2.Project.Variable is

   -----------
   -- Image --
   -----------

   overriding function Image
     (Self     : Object;
      Name_Len : Natural := 0) return String
   is

      use Ada.Strings.Unbounded;
      use GPR2.Project.Registry.Attribute;
      use all type GPR2.Project.Name_Values.Object;

      Name   : constant String := String (Self.Name);
      Result : Unbounded_String := To_Unbounded_String (Name);
   begin
      if Name_Len > 0 and then Name'Length < Name_Len then
         Append (Result, (Name_Len - Name'Length) * ' ');
      end if;

      Append (Result, " := ");

      case Self.Kind is
         when Single =>
            Append (Result, '"' & Self.Value.Text & '"');

         when List =>
            declare
               First : Boolean := True;
            begin
               Append (Result, '(');

               for V of Self.Values loop
                  if not First then
                     Append (Result, ", ");
                  end if;

                  Append (Result, '"' & String (V.Text) & '"');
                  First := False;
               end loop;

               Append (Result, ')');
            end;
      end case;

      Append (Result, ';');

      return To_String (Result);
   end Image;

end GPR2.Project.Variable;
