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

package GPRname.Pattern.Language is

   type Object is new GPRname.Pattern.Object with private;
   --  Extension of Compiled_Pattern_Object in which we associate a language
   --  to the pattern.

   function Create
     (Pattern  : Pattern_Type;
      Language : Language_Type) return Object'Class;
   --  Constructor for Compiled_Pattern_Language_Object

   function Language (Self : Object) return Language_Type;
   --  Returns the language for Self

private

   type Object is new GPRname.Pattern.Object with record
      Language : Unbounded_String;
   end record;

end GPRname.Pattern.Language;
