--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

   function Image
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

end GPR2.Project.Variable;
