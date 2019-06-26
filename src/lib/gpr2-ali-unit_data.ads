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

with GPR2.ALI.With_Data.List;

package GPR2.ALI.Unit_Data is

   --
   --  Units (U lines)
   --

   type Unit_Type is (Is_Spec, Is_Body, Is_Spec_Only, Is_Body_Only);
   --  Indicates type of entry, if both body and spec appear in the ALI file,
   --  then the first unit is marked Is_Body, and the second is marked Is_Spec.
   --  If only a spec appears, then it is marked as Is_Spec_Only, and if only
   --  a body appears, then it is marked Is_Body_Only).

   type Unit_Kind is (Kind_Package, Kind_Subprogram);
   --  Indicates whether a library unit is a package or a subprogram

   type Flag is
     (Preelab,
      No_Elab_Code,
      Pure,
      Dynamic_Elab,
      Elaborate_Body,
      Has_RACW,
      Remote_Types,
      Shared_Passive,
      RCI,
      Predefined,
      Is_Generic,
      Init_Scalars,
      SAL_Interface,
      Body_Needed_For_SAL,
      Elaborate_Body_Desirable);
   --  Flags that can appear on a unit line

   function Image (F : Flag) return String;
   --  Return string image of F (with nice mixed casing)

   type Flag_Array is array (Flag) of Boolean with Pack;
   --  Set of flags applying to a given unit

   type Object is tagged private;

   function Create
     (Uname : Name_Type;
      Sfile : Simple_Name;
      Utype : Unit_Type) return Object;
   --  Creates and returns a Unit_Data object

   function Sfile (Self : Object) return Simple_Name;
   --  Returns the Sfile for Self

   function Uname (Self : Object) return Name_Type;
   --  Returns the Uname for Self

   function Utype (Self : Object) return Unit_Type;
   --  Returns the Utype for Self

   function Withs (Self : Object) return With_Data.List.Object;
   --  Returns the list of With_Data objects for Self

   function Flags (Self : Object) return Flag_Array;
   --  Returns the flags for Self

   function Kind (Self : Object) return Unit_Kind;
   --  Returns the unit kind for Self

   procedure Add_With (Self : in out Object; W : With_Data.Object);
   --  Add the With_Data object W to Self

   Default_Flags : constant Flag_Array;

   procedure Set_Flags (Self : in out Object; Flags : Flag_Array);
   --  Sets the boolean flags for Self to the given Flags

   procedure Set_Kind (Self : in out Object; Kind : Unit_Kind);
   --  Sets the Unit_Kind for Self

   procedure Set_Utype (Self : in out Object; Utype : Unit_Type);
   --  Sets the Utype for Self

private

   type Object is tagged record

      Uname : Unbounded_String;
      --  Name of Unit

      Sfile : Unbounded_String;
      --  Name of source file

      Utype : Unit_Type;
      --  Type of entry

      Withs : With_Data.List.Object;
      --  Withs for this file

      Flags : Flag_Array;
      --  Unit flags

      Kind  : Unit_Kind;
      --  Indicates the nature of the unit
   end record;

   Default_Flags : constant Flag_Array := (others => False);

   function Create
     (Uname : Name_Type;
      Sfile : Simple_Name;
      Utype : Unit_Type) return Object is
     (Object'(Uname     => +String (Uname),
              Sfile     => +String (Sfile),
              Utype     => Utype,
              Withs     => With_Data.List.Empty_List,
              Flags     => Default_Flags,
              Kind      => Kind_Package));

   function Sfile (Self : Object) return Simple_Name is
     (Simple_Name (-Self.Sfile));

   function Uname (Self : Object) return Name_Type is
     (Name_Type (-Self.Uname));

   function Utype (Self : Object) return Unit_Type is
     (Self.Utype);

   function Withs (Self : Object) return With_Data.List.Object is
     (Self.Withs);

   function Flags (Self : Object) return Flag_Array is
     (Self.Flags);

   function Kind (Self : Object) return Unit_Kind is
     (Self.Kind);

end GPR2.ALI.Unit_Data;
