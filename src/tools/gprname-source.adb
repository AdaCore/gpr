------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

package body GPRname.Source is

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Object) return Boolean is
     (Path_Name."<" (Left.File, Right.File));

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Object) return Boolean is
     (Path_Name."=" (Left.File, Right.File));

   ------------
   -- Create --
   ------------

   function Create
     (File : Path_Name.Object; Language : Language_Type) return Object is
     (Object'(Unit_Based => False,
              File       => File,
              Language   => +String (Language)));

   -----------------------
   -- Create_Unit_Based --
   -----------------------

   function Create_Unit_Based
     (File : Path_Name.Object; Language : Language_Type) return Object is
     (Object'(Unit_Based => True,
              File       => File,
              Language   => +String (Language),
              Units      => <>));

   -----------------
   -- Append_Unit --
   -----------------

   procedure Append_Unit (Self : in out Object; U : Unit.Object) is
   begin
      Self.Units.Append (U);
   end Append_Unit;

   ----------
   -- File --
   ----------

   function File (Self : Object) return Path_Name.Object is
     (Self.File);

   --------------
   -- Language --
   --------------

   function Language (Self : Object) return Language_Type is
     (Language_Type (To_String (Self.Language)));

   -----------
   -- Units --
   -----------

   function Units (Self : Object) return Unit.Vector.Object is
     (Self.Units);

end GPRname.Source;
