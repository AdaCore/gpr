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

with GNATCOLL.Refcount;
with GNATCOLL.Tribooleans;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name.Set;

package GPR2.Project is

   use GNATCOLL.Tribooleans;

   --  This package is the root of the high level abstraction of a hierarchy of
   --  projects given by a root project.

   type Standalone_Library_Kind is (No, Standard, Encapsulated);

   --
   --  Iterators
   --

   type Iterator_Kind is
     (I_Project, I_Extended, I_Imported, I_Aggregated, I_Recursive);
   type Iterator_Control is array (Iterator_Kind) of Boolean  with Pack;

   Default_Iterator : constant Iterator_Control;

   type Filter_Kind is
     (F_Standard, F_Library, F_Abstract, F_Aggregate, F_Aggregate_Library);
   type Filter_Control is array (Filter_Kind) of Boolean with Pack;

   Default_Filter : constant Filter_Control;
   Library_Filter : constant Filter_Control;

   type Status_Kind is
     (S_Externally_Built);
   type Status_Control is array (Status_Kind) of Triboolean;

   Default_Status : constant Status_Control;

   function Create
     (Name  : Name_Type;
      Paths : Path_Name.Set.Object := Path_Name.Set.Set.Empty_List)
      return Path_Name.Object;
   --  Given a filename (possibly a full pathname) returns a Path_Name_Type. If
   --  Name is not an absolute path name it is looked into Paths.

   function Search_Paths
     (Root_Project      : Path_Name.Object;
      Tree_Search_Paths : Path_Name.Set.Object) return Path_Name.Set.Object
     with Pre  => Root_Project.Is_Defined
                  and then not Tree_Search_Paths.Is_Empty,
          Post => not Search_Paths'Result.Is_Empty;
   --  Returns the project search path for the given project and the give tree

   function Look_For_Default_Project return Path_Name.Object;
   --  Look for default project in the current directory, return Undefined if
   --  not found.

private

   type Relation_Status is (Root, Imported, Aggregated);

   type Definition_Base (Has_Context : Boolean) is abstract tagged record
      Id                : Natural := 0;
      Externals         : Containers.Name_List;
      --  List of externals directly or indirectly visible
      Signature         : Context.Binary_Signature :=
                            Context.Default_Signature;
      Status            : Relation_Status := Root;
      Kind              : Project_Kind;
      Sources_Signature : Context.Binary_Signature :=
                            Context.Default_Signature;

      case Has_Context is
         when True =>
            Context   : GPR2.Context.Object; -- root context
            A_Context : GPR2.Context.Object; -- aggregate context
         when False =>
            null;
      end case;
   end record;

   package Definition_References is new GNATCOLL.Refcount.Shared_Pointers
     (Definition_Base'Class);

   Default_Iterator : constant Iterator_Control := (others => True);

   Default_Filter   : constant Filter_Control := (others => True);

   Library_Filter   : constant Filter_Control :=
                        (F_Library | F_Aggregate_Library => True,
                         others                          => False);

   Default_Status : constant Status_Control := (others => Indeterminate);

end GPR2.Project;
