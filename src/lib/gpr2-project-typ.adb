--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Containers;

package body GPR2.Project.Typ is

   -----------
   -- Image --
   -----------

   function Image
     (Self     : Object;
      Name_Len : Natural := 0) return String
   is
      Name   : constant String := String (Self.Name.Text);
      Result : Unbounded_String := To_Unbounded_String ("type ");
   begin
      Append (Result, Name);

      if Name_Len > 0 and then Name'Length < Name_Len then
         Append (Result, (Name_Len - Name'Length) * ' ');
      end if;

      Append (Result, " is ");
      Append (Result, Containers.Image (Self.Values));

      Append (Result, ';');

      return To_String (Result);
   end Image;

end GPR2.Project.Typ;
