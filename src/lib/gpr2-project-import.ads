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

with GPR2.Source_Reference;

package GPR2.Project.Import is

   use type GPR2.Source_Reference.Object;

   type Object is new Source_Reference.Object with private;

   subtype Import_Type is Object;

   Undefined : constant Object;

   function Create
     (Path_Name  : Path_Name_Type;
      Sloc       : Source_Reference.Object;
      Is_Limited : Boolean) return Object
     with Pre => Sloc /= Source_Reference.Undefined;

   function Path_Name (Self : Object) return Path_Name_Type
     with Pre => Self /= Undefined;
   --  Full pathname of the corresponding project file

   function Is_Limited (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if this is a limited import

private

   type Object is new Source_Reference.Object with record
      Path_Name  : Path_Name_Type;
      Is_Limited : Boolean;
   end record;

   Undefined : constant Object :=
                 (Source_Reference.Undefined
                  with Path_Name => No_Path_Name, Is_Limited => False);

end GPR2.Project.Import;
