------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
     (Filename     : Full_Path_Name;
      Line, Column : Natural) return Object'Class is
   begin
      return Object'(Line, Column, To_Unbounded_String (Filename));
   end Create;

   --------------
   -- Filename --
   --------------

   function Filename (Self : Object) return Full_Path_Name is
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
