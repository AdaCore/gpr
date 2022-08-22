--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers;

with GNATCOLL.Refcount;
with GNATCOLL.Tribooleans;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name.Set;

private with GPR2.Source_Reference.Value;

package GPR2.Project is

   use GNATCOLL.Tribooleans;

   use type Ada.Containers.Count_Type;

   --  This package is the root of the high level abstraction of a hierarchy of
   --  projects given by a root project.

   type Standalone_Library_Kind is (No, Standard, Encapsulated, Full);

   --
   --  Iterators
   --

   type Iterator_Kind is
     (I_Project,
      I_Extended,
      I_Imported,
      I_Aggregated,
      I_Recursive,
      I_Runtime,
      I_Configuration);
   type Iterator_Control is array (Iterator_Kind) of Boolean  with Pack;

   Default_Iterator : constant Iterator_Control;
   --  By default iterate on the whole tree except runtime and configuration
   --  views

   Full_Iterator    : constant Iterator_Control;
   --  Iterates on the whole project tree, including config and runtime views
   --  if present.

   Config_File_Extension  : constant Filename_Type := ".cgpr";
   Project_File_Extension : constant Filename_Type := ".gpr";
   --  The standard config and user project file name extensions

   Config_File_Extension_No_Dot : Filename_Type
     renames Config_File_Extension (2 .. Config_File_Extension'Last);

   Project_File_Extension_No_Dot : Filename_Type
     renames Project_File_Extension (2 .. Project_File_Extension'Last);

   Default_Config_Name : constant Filename_Type := "default.cgpr";

   type Filter_Kind is
     (F_Standard, F_Library, F_Abstract, F_Aggregate, F_Aggregate_Library);

   type Filter_Control is array (Filter_Kind) of Boolean with Pack;

   Default_Filter : constant Filter_Control;
   Library_Filter : constant Filter_Control;

   type Status_Kind is (S_Externally_Built);

   type Status_Control is array (Status_Kind) of Triboolean;

   Default_Status : constant Status_Control;

   function Create
     (Name  : Filename_Type;
      Paths : Path_Name.Set.Object := Path_Name.Set.Empty_Set)
      return Path_Name.Object
     with Post => Create'Result.Is_Defined;
   --  Given a filename (possibly a full pathname) returns a Path_Name_Type. If
   --  Name is not an absolute path name it is looked into Paths.

   function Search_Paths
     (Root_Project      : Path_Name.Object;
      Tree_Search_Paths : Path_Name.Set.Object) return Path_Name.Set.Object
     with Pre  => Root_Project.Is_Defined
                  and then not Tree_Search_Paths.Is_Empty,
          Post => not Search_Paths'Result.Is_Empty;
   --  Returns the project search path for the given project and the given tree

   function Ensure_Extension
     (Name        : Filename_Type;
      Config_File : Boolean := False) return Filename_Type;
   --  If Name ending with ".gpr" or ".cgpr" the function returns it unchanged,
   --  otherwise returns Name appended with ".gpr" suffix or ".cgpr" if
   --  Config_File is set.

   function Default_Search_Paths
     (Current_Directory : Boolean) return Path_Name.Set.Object
     with Post => Default_Search_Paths'Result.Length > 0;
   --  Get the search paths common for all targets.
   --  If Current_Directory is True then the current directory is incuded at
   --  the first place in the result set.

   procedure Append_Default_Search_Paths (Paths : in out Path_Name.Set.Object);
   --  Add Default_Search_Paths without current directory to the Paths
   --  parameter.

private

   type Definition_Base is abstract tagged record
      Id                : Natural := 0;
      Path              : Path_Name.Object;
      Externals         : Containers.Name_List;
      --  List of externals directly or indirectly visible
      Signature         : Context.Binary_Signature :=
                            Context.Default_Signature;
      Is_Root           : Boolean := False;
      --  A view will have Is_Root set to True only of root of tree. In
      --  practice three views are considered as root projects: the view
      --  associated with the loaded main project file, the config view and
      --  the runtime view.
      Kind              : Project_Kind;
      Sources_Signature : Context.Binary_Signature :=
                            Context.Default_Signature;
   end record;

   package Definition_References is new GNATCOLL.Refcount.Shared_Pointers
     (Definition_Base'Class);

   subtype Weak_Reference is Definition_References.Weak_Ref;

   Default_Iterator : constant Iterator_Control := (I_Runtime       => False,
                                                    I_Configuration => False,
                                                    others          => True);

   Full_Iterator    : constant Iterator_Control := (others => True);

   Default_Filter   : constant Filter_Control := (others => True);

   Library_Filter   : constant Filter_Control :=
                        (F_Library | F_Aggregate_Library => True,
                         others                          => False);

   Default_Status : constant Status_Control := (others => Indeterminate);

   function At_Pos_Or
     (Value   : Source_Reference.Value.Object'Class;
      Default : Unit_Index) return Unit_Index
   is
     (if Value.Is_Defined and then Value.Has_At_Pos
      then Value.At_Pos
      else Default);
   --  Returns At_Pos if defined or Default if not defined

end GPR2.Project;
