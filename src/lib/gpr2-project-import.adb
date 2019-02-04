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

package body GPR2.Project.Import is

   ------------
   -- Create --
   ------------

   function Create
     (Path_Name  : GPR2.Path_Name.Object;
      Sloc       : Source_Reference.Object;
      Is_Limited : Boolean) return Object is
   begin
      return Object'(Sloc with Path_Name, Is_Limited);
   end Create;

   ----------------
   -- Is_Limited --
   ----------------

   function Is_Limited (Self : Object) return Boolean is
   begin
      return Self.Is_Limited;
   end Is_Limited;

   ---------------
   -- Path_Name --
   ---------------

   function Path_Name (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Path_Name;
   end Path_Name;

end GPR2.Project.Import;
