--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Ordered_Sets;

with GNATCOLL.Refcount;

--  with GPR2.Build.Db.Source_Sets;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.View;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Object_Info;
with GPR2.Build.Source;
with GPR2.Source_Reference.Value;
with GPR2.View_Ids;

limited with GPR2.Build.View_Db;
limited with GPR2.Build.Tree_Db;

private package GPR2.Build.View_Tables is

   use type Ada.Containers.Hash_Type;
   use type Path_Name.Object;

   --  generic file object: can represent any file found on the filesystem

   type File_Info is record
      Path    : Path_Name.Object;
      --  Full path to the file
      Stamp   : Ada.Calendar.Time;
      --  Modification time at the moment we've read the source dir
      Dir_Ref : Source_Reference.Value.Object;
      --  Attribute value that made us read this file
   end record;

   overriding function "=" (F1, F2 : File_Info) return Boolean is
     (F1.Path = F2.Path);

   function "<" (F1, F2 : File_Info) return Boolean is
      (F1.Path < F2.Path);

   function Hash (F : File_Info) return Ada.Containers.Hash_Type is
      (F.Path.Hash);

   package File_Info_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Simple_Name, File_Info, Hash, "=");

   package File_Sets is new Ada.Containers.Ordered_Sets
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

   package Src_Info_Maps is new Ada.Containers.Hashed_Maps
     (Path_Name.Object, Build.Source.Object,
      Path_Name.Hash, Path_Name."=", Source."=");

   package Object_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Simple_Name, Object_Info.Object, Hash, "=", Object_Info."=");

   type Source_Proxy is record
      View      : GPR2.Project.View.Object;
      Path_Name : GPR2.Path_Name.Object;
      Inh_From  : GPR2.Project.View.Object;
   end record;

   No_Proxy : constant Source_Proxy := (others => <>);

   function Hash (Proxy : Source_Proxy) return Ada.Containers.Hash_Type
     is (Path_Name.Hash (Proxy.Path_Name) + View_Ids.Hash (Proxy.View.Id));

   package Source_Proxy_Sets is new Ada.Containers.Hashed_Sets
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
      Object_Infos    : Object_Maps.Map;
      --  Object traceability information table
      Object_Files    : File_Info_Maps.Map;
      --  raw list of object files, used to check updates
      Ali_Files       : File_Info_Maps.Map;
      --  raw list of ali files, used to check updates

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

   procedure Add_Source
     (Data               : in out View_Data;
      View_Owner         : GPR2.Project.View.Object;
      Path               : GPR2.Path_Name.Object;
      Extended_View      : GPR2.Project.View.Object;
      Resolve_Visibility : Boolean := False);

   procedure Remove_Source
     (Data               : in out View_Data;
      View_Owner         : GPR2.Project.View.Object;
      Path               : GPR2.Path_Name.Object;
      Extended_View      : GPR2.Project.View.Object;
      Resolve_Visibility : Boolean := False);

   procedure Refresh
     (Data     : in out View_Data;
      Messages : in out GPR2.Log.Object);

   package Data_Refs is new GNATCOLL.Refcount.Shared_Pointers (View_Data);

   subtype View_Data_Ref is Data_Refs.Reference_Type;

   View_Base_For : access function (Data : View_Data) return View_Db.Object;
   Get_Ref       : access function
                     (Obj : View_Db.Object) return View_Data_Ref;

   function Get_Data
     (Db   : access GPR2.Build.Tree_Db.Object;
      View : GPR2.Project.View.Object) return View_Data_Ref;

end GPR2.Build.View_Tables;
