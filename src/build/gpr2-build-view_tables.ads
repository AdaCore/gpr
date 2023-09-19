--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Ordered_Sets;

with GNATCOLL.Refcount;

with GPR2.Containers;
with GPR2.Log;
with GPR2.Project.View.Set;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Object_Info;
with GPR2.Build.Source;
with GPR2.Source_Reference.Value;
with GPR2.View_Ids;

limited with GPR2.Build.View_Db;
limited with GPR2.Build.Tree_Db;

private package GPR2.Build.View_Tables is

   use type Ada.Containers.Hash_Type;

   --  generic file object: can represent any file found on the filesystem

   type File_Info (Path_Len : Natural) is record
      Stamp   : Ada.Calendar.Time;
      --  Modification time at the moment we've read the source dir
      Dir_Ref : Source_Reference.Value.Object;
      --  Attribute value that made us read this file
      Path    : Filename_Optional (1 .. Path_Len);
      --  Full path to the file
   end record;

   overriding function "=" (F1, F2 : File_Info) return Boolean is
     (F1.Path = F2.Path);

   function "<" (F1, F2 : File_Info) return Boolean is
      (F1.Path < F2.Path);

   function Hash (F : File_Info) return Ada.Containers.Hash_Type is
      (GPR2.Hash (F.Path));

   package File_Info_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Simple_Name, File_Info, Hash, "=");

   package File_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (File_Info);

   package Basename_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Simple_Name, Hash, "=");

   package Name_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Name_Type, Name_Type, Hash, "=");

   --  Source handling:
   --  The sources in the Database are updated by delta comparison with
   --  the previous list of sources, to speed up the load of a project from
   --  an existing database.
   --
   --  To handle efficiently such update, and handle properly the visibility
   --  of the sources, as well as ensuring data consistency accross views,
   --  the following structures are used:
   --  * Self.Src_Files: this is the simplest structure as it just represents
   --    the file entries in the various source directories declared in the
   --    view. This list is used to know files deleted/modified/added between
   --    two updates of the database.
   --  * Self.Src_Infos: the view's own sources (e.g. sources found in the
   --    view's source directories. The values contain various information
   --    such as source language, kind, etc. This is the only table that has
   --    this information: all other tables will contain references to this
   --    table.
   --  * Self.Sources: indexed by simple names, points to the actual source
   --    info object for each simple name.
   --  * Self.Hidden_Sources: indexed by simple names, points to a list of
   --    sources that are not visible (because of overloading or errors).

   package Src_Info_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Filename_Optional, Build.Source.Object,
      Hash, "=", Source."=");

   package Object_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Simple_Name, Object_Info.Object, Hash, "=", Object_Info."=");

   type Source_Proxy (Path_Len : Natural) is record
      View      : GPR2.Project.View.Object;
      --  The view that owns the source (e.g. that has the source via its
      --  source directory or source list)
      Inh_From  : GPR2.Project.View.Object;
      --  Points to the extended view if any, from which the current view
      --  inherits the source
      Path_Name : Filename_Optional (1 .. Path_Len);
      --  The source path name
   end record;

   No_Proxy : constant Source_Proxy := (Path_Len => 0, others => <>);

   function Hash (Proxy : Source_Proxy) return Ada.Containers.Hash_Type
     is (GPR2.Hash (Proxy.Path_Name) + View_Ids.Hash (Proxy.View.Id));

   package Source_Proxy_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Source_Proxy, Hash, "=");
   --  A set of source reference

   package Basename_Source_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Simple_Name, Source_Proxy, Hash, "=");
   --  Basename to source reference maps

   package Basename_Source_List_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Simple_Name, Source_Proxy_Sets.Set, Hash, "=", Source_Proxy_Sets."=");
   --  Basename to source reference list

   package Compilation_Unit_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Name_Type, Compilation_Unit.Object, Hash, "=", Compilation_Unit."=");

   package Sources_By_Langs_Maps is new Ada.Containers.Hashed_Maps
     (GPR2.Language_Id, Natural, Hash, "=");

   package Unit_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Name_Type,
      GPR2.Project.View.Set.Object,
      GPR2.Hash,
      GPR2."=",
      GPR2.Project.View.Set."=");

   package Source_Set renames GPR2.Containers.Filename_Type_Set;

   type View_Data (Is_Root : Boolean) is record
      --  Static data:

      Tree_Db         : access GPR2.Build.Tree_Db.Object;

      View            : GPR2.Project.View.Object;
      --  Owning view

      --  Dynamic data:

      Sources         : Basename_Source_Maps.Map;
      --  Sources to take into account for View after visibility is resolved.
      Overloaded_Srcs : Basename_Source_List_Maps.Map;
      --  Keeps track of source overloading, to reassess source visibility or
      --  erroneous cases when a source is added or removed.
      Src_Infos       : Src_Info_Maps.Map;
      --  Sources identified in the source directories of the view, indexed
      --  by their full names.
      Src_Files       : File_Sets.Set;
      --  raw list of files found in the source directories, used to check
      --  updates
      Langs_Usage     : Sources_By_Langs_Maps.Map;
      --  Indicates the number of sources per language. Allows detecting
      --  unused languages.
      Object_Infos    : Object_Maps.Map;
      --  Object traceability information table
      Object_Files    : File_Info_Maps.Map;
      --  raw list of object files, used to check updates
      Ali_Files       : File_Info_Maps.Map;
      --  raw list of ali files, used to check updates
      Listed_Sources   : Source_Set.Set;
      --  content of Source_Files and Source_List_File attributes
      Excluded_Sources : Source_Set.Set;
      --  content of Excluded_Source_Files and Excluded_Source_List_File
      --  attributes.

      Own_CUs         : Unit_Maps.Map;
      --  The compilation units whose main unit belongs to the table's view

      case Is_Root is
         when True =>
            --  Compilation units are stored in root projects: so for aggregate
            --  projects each root project will maintain its own list so that
            --  the unit names don't clash between otherwise independent
            --  subtrees.
            CUs       : Compilation_Unit_Maps.Map;
            --  List of compilation units, indexed by their identifier
            Separates : Name_Maps.Map;
            --  Map of separates to their declaring unit. Note that the
            --  parent unit may not be a compilation unit but another separate.
         when False =>
            null;
      end case;
   end record;

   package Data_Refs is new GNATCOLL.Refcount.Shared_Pointers (View_Data);

   subtype View_Data_Ref is Data_Refs.Reference_Type;

   procedure Check_Source_Lists
     (Data     : View_Data_Ref;
      Messages : in out GPR2.Log.Object);
   --  Check the attributes Source_File_List, Source_Files,
   --  Excluded_Source_File_List and Excluded_Source_Files and fill the
   --  corresponding structures in Data.
   --  To be called before refreshing the sources.

   procedure Add_Source
     (Data               : View_Data_Ref;
      View_Owner         : GPR2.Project.View.Object;
      Path               : Filename_Type;
      Extended_View      : GPR2.Project.View.Object;
      Resolve_Visibility : Boolean := False;
      Messages           : in out GPR2.Log.Object)
     with Inline;

   procedure Remove_Source
     (Data               : View_Data_Ref;
      View_Owner         : GPR2.Project.View.Object;
      Path               : Filename_Type;
      Extended_View      : GPR2.Project.View.Object;
      Resolve_Visibility : Boolean := False;
      Messages           : in out GPR2.Log.Object)
     with Inline;

   procedure Refresh
     (Data     : View_Data_Ref;
      Messages : in out GPR2.Log.Object);

   View_Base_For : access function
                     (Data : View_Data) return View_Db.Object;
   Get_Ref       : access function
                     (Obj : View_Db.Object) return View_Data_Ref;

   function Get_Data
     (Db   : access GPR2.Build.Tree_Db.Object;
      View : GPR2.Project.View.Object) return View_Data_Ref;

end GPR2.Build.View_Tables;
