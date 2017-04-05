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

with GPR2.Containers;

package GPR2.Project is

   --  This package is the root of the high level abstraction of a hierarchy of
   --  projects given by a root project.

   --
   --  Iterators
   --

   type Iterator_Kind is mod 2 ** 5;

   I_Invalid    : constant Iterator_Kind;
   I_Project    : constant Iterator_Kind;
   I_Extended   : constant Iterator_Kind;
   I_Imported   : constant Iterator_Kind;
   I_Aggregated : constant Iterator_Kind;
   I_Recursive  : constant Iterator_Kind;
   --  ?? missing extended

   I_Default   : constant Iterator_Kind;

   function Is_Set (Set, Kind : Iterator_Kind) return Boolean
     is ((Set and Kind) = Kind);

   type Project_Filter is mod 2 ** 8;

   F_Invalid           : constant Project_Filter;
   F_Standard          : constant Project_Filter;
   F_Library           : constant Project_Filter;
   F_Abstract          : constant Project_Filter;
   F_Aggregate         : constant Project_Filter;
   F_Aggregate_Library : constant Project_Filter;

   F_Default : constant Project_Filter;

   function Is_Set (Set, Kind : Project_Filter) return Boolean
     is ((Set and Kind) = Kind);

   function Paths (Parent : Path_Name_Type) return Containers.Name_List;
   --  Returns the list of search paths for imported projects in Parent. Parent
   --  is No_Path_Name for the root project.

   function Create
     (Name  : Name_Type;
      Paths : Containers.Name_List := Containers.Name_Type_List.Empty_Vector)
      return Path_Name_Type;
   --  Given a filename (possibly a full pathname) return a Path_Name_Type. If
   --  Name is not an absolute path name it is looked into Paths.

private

   I_Invalid    : constant Iterator_Kind := 2#00000#;
   I_Project    : constant Iterator_Kind := 2#00001#;
   I_Extended   : constant Iterator_Kind := 2#00010#;
   I_Imported   : constant Iterator_Kind := 2#00100#;
   I_Aggregated : constant Iterator_Kind := 2#01000#;
   I_Recursive  : constant Iterator_Kind := 2#10000#;

   I_Default   : constant Iterator_Kind :=
                   I_Project or I_Imported or I_Aggregated or I_Recursive;

   F_Invalid           : constant Project_Filter := 2#00000000#;
   F_Standard          : constant Project_Filter := 2#00000001#;
   F_Library           : constant Project_Filter := 2#00000010#;
   F_Abstract          : constant Project_Filter := 2#00000100#;
   F_Aggregate         : constant Project_Filter := 2#00001000#;
   F_Aggregate_Library : constant Project_Filter := 2#00010000#;

   F_Default : constant Project_Filter :=
                 F_Standard or F_Library or F_Abstract
                 or F_Aggregate or F_Aggregate_Library;

end GPR2.Project;
