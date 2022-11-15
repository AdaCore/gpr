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

with GNAT.Regexp;

with GPRname.Common;

package GPRname.Pattern is

   use GNAT;

   use GPRname.Common;

   type Object is tagged private;
   --  We introduce this type to make for the fact that GNAT.Regexp doesn't
   --  give acces to the original string used to compile.

   function Create (Pattern : Pattern_Type) return Object'Class;
   --  Creates a Pattern

   function Pattern (Self : Object) return Pattern_Type;
   --  Returns the raw string used to compile Self

   function Regexp (Self : Object) return GNAT.Regexp.Regexp;
   --  Returns the compiled regex for Self

private

   use Ada.Strings.Unbounded;

   type Object is tagged record
      Pattern  : Unbounded_String;
      Regex    : GNAT.Regexp.Regexp;
   end record;

   function Pattern (Self : Object) return Pattern_Type is
     (Pattern_Type (To_String (Self.Pattern)));

   function Regexp (Self : Object) return GNAT.Regexp.Regexp is
     (Self.Regex);

end GPRname.Pattern;
