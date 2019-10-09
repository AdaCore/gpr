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

with GPR2.Source;

package GPR2.ALI.Withed_Unit is

   --
   --  Withed units (W and Z lines below a U line)
   --

   type Object is tagged private;

   function Create
     (Uname                            : Name_Type;
      Ukind                            : Kind_Type;
      Sfile                            : Optional_Name_Type;
      Afile                            : Optional_Name_Type;
      Implicit_With_From_Instantiation : Boolean) return Object;
   --  Creates and returns a With_Data object

   function Uname (Self : Object) return Name_Type;
   --  Returns the Uname for Self

private

   type Object is tagged record
      Uname : Unbounded_String;
      --  Name of Unit
      Ukind : Kind_Type;
      --  Kind
      Sfile : Unbounded_String;
      --  Name of source file, empty in generic case
      Afile : Unbounded_String;
      --  Name of ALI file, empty in generic case
      Implicit_With_From_Instantiation : Boolean := False;
      --  True if this is an implicit with from a generic instantiation
   end record;

   function Create
     (Uname : Name_Type;
      Ukind : Kind_Type;
      Sfile : Optional_Name_Type;
      Afile : Optional_Name_Type;
      Implicit_With_From_Instantiation : Boolean) return Object
   is
     (Object'(Uname => +String (Uname),
              Ukind => Ukind,
              Sfile => +String (Sfile),
              Afile => +String (Afile),
              Implicit_With_From_Instantiation =>
                 Implicit_With_From_Instantiation));

   function Uname (Self : Object) return Name_Type is
     (Name_Type (-Self.Uname));

end GPR2.ALI.Withed_Unit;
