------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2018, Free Software Foundation, Inc.          --
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

with GPR2.Path_Name;

limited with GPR2.Parser.Project;

private package GPR2.Parser.Registry is

   use type GPR2.Path_Name.Object;

   function Exists (Pathname : Path_Name.Object) return Boolean
     with Pre  => Pathname /= Path_Name.Undefined;
   --  Returns True if the project file given by its full path-name is known in
   --  the registry.

   procedure Register
     (Pathname : Path_Name.Object; Project : Parser.Project.Object)
     with Pre  => Pathname /= Path_Name.Undefined,
          Post => Exists (Pathname);
   --  Register a new project syntactic tree for Pathname or increment the
   --  reference count for the given project as this object can be shared
   --  by multiple project tree.

   procedure Unregister (Pathname : Path_Name.Object)
     with Pre => Exists (Pathname);
   --  Unregister the given project, remove it from the store if no more
   --  referenced (reference count is 0).

   function Get (Pathname : Path_Name.Object) return Project.Object
     with Pre => Exists (Pathname);
   --  Returns the syntactic tree for project Pathname

end GPR2.Parser.Registry;
