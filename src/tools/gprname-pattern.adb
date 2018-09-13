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

package body GPRname.Pattern is

   ------------
   -- Create --
   ------------

   function Create (Pattern : Pattern_Type) return Object is
   begin
      return (Object'(Pattern => +String (Pattern),
                      Regex   => Compile (String (Pattern), Glob => True)));
   exception
      when Error_In_Regexp =>
         raise GPRname_Exception
           with "invalid regular expression """ & String (Pattern) & """";
   end Create;

   -------------
   -- Pattern --
   -------------

   function Pattern (Self : Object) return Pattern_Type is
     (Pattern_Type (To_String (Self.Pattern)));

   -----------
   -- Regex --
   -----------

   function Regex (Self : Object) return Regexp is
     (Self.Regex);

end GPRname.Pattern;
