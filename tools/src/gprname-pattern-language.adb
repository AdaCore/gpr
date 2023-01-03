------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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

package body GPRname.Pattern.Language is

   ------------
   -- Create --
   ------------

   function Create
     (Pattern  : Pattern_Type;
      Language : Language_Type) return Object'Class is
   begin
      return Object'(GPRname.Pattern.Object (Create (Pattern))
                     with Language => +String (Language));
   end Create;

   --------------
   -- Language --
   --------------

   function Language (Self : Object) return Language_Type is
     (Language_Type (To_String (Self.Language)));

end GPRname.Pattern.Language;
