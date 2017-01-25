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

limited with GPR2.Parser.Project;

private package GPR2.Parser.Registry is

   function Exists (Pathname : Path_Name_Type) return Boolean;
   --  Returns True if the project file given by its full path-name is known in
   --  the registry.

   procedure Register
     (Pathname : Path_Name_Type; Project : Parser.Project.Object)
     with
       Pre  => not Exists (Pathname),
       Post => Exists (Pathname);
   --  Register a new project syntactic tree for Pathname

   function Get (Pathname : Path_Name_Type) return Project.Object
     with Pre => Exists (Pathname);
   --  Returns the syntactic tree for project Pathname

end GPR2.Parser.Registry;
