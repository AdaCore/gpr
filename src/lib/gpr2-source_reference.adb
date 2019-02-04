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

package body GPR2.Source_Reference is

   ------------
   -- Column --
   ------------

   function Column (Self : Object) return Positive is
   begin
      return Self.Column;
   end Column;

   ------------
   -- Create --
   ------------

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural) return Object'Class is
   begin
      return Object'(Line, Column, To_Unbounded_String (Filename));
   end Create;

   --------------
   -- Filename --
   --------------

   function Filename (Self : Object) return Path_Name.Full_Name is
   begin
      return To_String (Self.Filename);
   end Filename;

   ----------
   -- Line --
   ----------

   function Line (Self : Object) return Positive is
   begin
      return Self.Line;
   end Line;

end GPR2.Source_Reference;
