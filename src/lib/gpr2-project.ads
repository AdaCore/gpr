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

with GPR2.Path_Name.Set;

package GPR2.Project is

   --  This package is the root of the high level abstraction of a hierarchy of
   --  projects given by a root project.

   --
   --  Iterators
   --

   type Iterator_Control is
     (I_Invalid, I_Project, I_Extended, I_Imported, I_Aggregated, I_Recursive);

   type Iterator_Kind is array (Iterator_Control) of Boolean  with Pack;

   Default_Iterator : constant Iterator_Kind;

   type Filter_Control is
     (F_Invalid, F_Standard, F_Library, F_Abstract,
      F_Aggregate, F_Aggregate_Library);

   type Project_Filter is array (Filter_Control) of Boolean with Pack;

   Default_Filter : constant Project_Filter;
   Library_Filter : constant Project_Filter;

   function Paths (Parent : Path_Name.Object) return Path_Name.Set.Object;
   --  Returns the list of search paths for imported projects in Parent. Parent
   --  is No_Path_Name for the root project.

   function Create
     (Name  : Name_Type;
      Paths : Path_Name.Set.Object := Path_Name.Set.Set.Empty_List)
      return Path_Name.Object;
   --  Given a filename (possibly a full pathname) return a Path_Name_Type. If
   --  Name is not an absolute path name it is looked into Paths.

private

   Default_Iterator : constant Iterator_Kind :=
                        (I_Project | I_Imported | I_Extended
                         | I_Aggregated | I_Recursive => True,
                         others                       => False);

   Default_Filter   : constant Project_Filter :=
                        (F_Standard | F_Library | F_Abstract
                         | F_Aggregate | F_Aggregate_Library => True,
                         others                              => False);

   Library_Filter   : constant Project_Filter :=
                        (F_Library | F_Aggregate_Library => True,
                         others                          => False);
end GPR2.Project;
