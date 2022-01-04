------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with GPR2;

with GPRname.Common;

package GPRname.Unit is

   use Ada.Strings.Unbounded;

   use GPRname.Common;

   type Object is tagged private;
   --  The relevant information for one Ada unit

   type Unit_Kind is (K_Spec, K_Body);

   function Create
     (Name            : GPR2.Name_Type;
      Kind            : Unit_Kind;
      Index_In_Source : Natural) return Object;
   --  Creates a Unit object

   function Name (Self : Object) return GPR2.Name_Type;
   --  Returns the unit name of Self

   function Kind (Self : Object) return Unit_Kind;
   --  Returns the unit kind for Self

   function Index_In_Source (Self : Object) return Natural;
   --  Returns the index for Self in the corresponding source file.
   --  Zero means the source is single-unit.

private

   type Object is tagged record
      Name            : Unbounded_String;
      Kind            : Unit_Kind;
      Index_In_Source : Natural := 0;
   end record;

   function Name (Self : Object) return GPR2.Name_Type is
     (GPR2.Name_Type (To_String (Self.Name)));

   function Kind (Self : Object) return Unit_Kind is
     (Self.Kind);

   function Index_In_Source (Self : Object) return Natural is
     (Self.Index_In_Source);

end GPRname.Unit;
