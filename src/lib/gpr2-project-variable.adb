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

package body GPR2.Project.Variable is

   ------------
   -- Create --
   ------------

   function Create
     (Name  : Source_Reference.Identifier.Object;
      Value : Source_Reference.Value.Object;
      Typ   : Project.Typ.Object) return Object is
   begin
      return Object'(Name_Values.Create (Name, Value) with Typ => Typ);
   end Create;

   function Create
     (Name   : Source_Reference.Identifier.Object;
      Values : Containers.Source_Value_List;
      Typ    : Project.Typ.Object) return Object is
   begin
      return Object'(Name_Values.Create (Name, Values) with Typ => Typ);
   end Create;

   overriding function Create
     (Name  : Source_Reference.Identifier.Object;
      Value : Source_Reference.Value.Object) return Object is
   begin
      return Object'
        (Name_Values.Create (Name, Value) with Project.Typ.Undefined);
   end Create;

   overriding function Create
     (Name   : Source_Reference.Identifier.Object;
      Values : Containers.Source_Value_List) return Object is
   begin
      return Object'
        (Name_Values.Create (Name, Values) with Project.Typ.Undefined);
   end Create;

   -----------
   -- Image --
   -----------

   overriding function Image
     (Self     : Object;
      Name_Len : Natural := 0) return String
   is

      use GPR2.Project.Registry.Attribute;
      use all type GPR2.Project.Name_Values.Object;

      Name   : constant String := String (Self.Name.Text);
      Result : Unbounded_String := To_Unbounded_String (Name);
   begin
      if Name_Len > 0 and then Name'Length < Name_Len then
         Append (Result, (Name_Len - Name'Length) * ' ');
      end if;

      if Self.Has_Type then
         Append (Result, " : ");
         Append (Result, String (Self.Typ.Name.Text));
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

   ------------
   -- Rename --
   ------------

   overriding function Rename
     (Self : Object;
      Name : Source_Reference.Identifier.Object) return Object is
   begin
      return Object'
        (Name_Values.Rename (Name_Values.Object (Self), Name) with Self.Typ);
   end Rename;

end GPR2.Project.Variable;
