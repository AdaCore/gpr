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

with GPR2.Path_Name;

limited with GPR2.Parser.Project;

private package GPR2.Parser.Registry is

   use type GPR2.Path_Name.Object;

   function Exists (Pathname : Path_Name.Object) return Boolean
     with Pre  => Pathname.Is_Defined;
   --  Returns True if the project file given by its full path-name is known in
   --  the registry.

   procedure Register
     (Pathname : Path_Name.Object; Project : Parser.Project.Object)
     with Pre  => Pathname.Is_Defined,
          Post => Exists (Pathname);
   --  Registers a new project syntactic tree for Pathname or increment the
   --  reference count for the given project as this object can be shared
   --  by multiple project tree.

   procedure Unregister (Pathname : Path_Name.Object)
     with Pre => Exists (Pathname);
   --  Unregisters the given project, removes it from the store if no longer
   --  referenced (reference count is 0).

   function Get (Pathname : Path_Name.Object) return Project.Object
     with Pre => Exists (Pathname);
   --  Returns the syntactic tree for project Pathname

end GPR2.Parser.Registry;
