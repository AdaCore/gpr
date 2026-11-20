--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;

with GNATCOLL.Refcount;

with GPR2.Containers;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.View.Set;
with GPR2.Project.View.Vector;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Compilation_Unit.Maps;
with GPR2.Build.Source_Base;
with GPR2.Build.Source;

limited with GPR2.Build.View_Db;
limited with GPR2.Build.Tree_Db;

private package GPR2.Build.View_Tables is

   subtype With_View_Db is GPR2.Project_Kind
     with Static_Predicate =>
       With_View_Db not in K_Aggregate | K_Configuration;

   package Name_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Name_Type, Name_Type, Hash, "=");

   --  Own source metadata is collected first. Sources and Basenames then
   --  describe the view after project extension and visibility resolution.

   package Src_Info_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Filename_Optional, Build.Source_Base.Object,
      Hash, "=", Source_Base."=");

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

   package Filename_Source_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Filename_Type, Source_Proxy, Hash, "=");
   --  Pathname to source reference maps

   package Basename_Source_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Simple_Name, Source_Proxy, Hash, "=");
   --  Basename to pathname maps

   type Visible_Source_Result is record
      Src       : Build.Source.Object;
      Ambiguous : Boolean := False;
   end record;

   package Visible_Source_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Simple_Name, Visible_Source_Result, Hash, "=");
   --  See View_Data.Visible_Src_Cache

   package Runtime_Override_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Simple_Name, Containers.Name_Set, Hash, "=",
      Containers.Name_Type_Set."=");

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

   type View_Data (Is_Root : Boolean) is record
      --  Static data:

      Tree_Db         : access GPR2.Build.Tree_Db.Object;

      View            : GPR2.Project.View.Object;
      --  Owning view

      --  Dynamic data

      Sources         : Filename_Source_Maps.Map;
      --  Own and inherited sources, including hidden sources.
      Basenames       : Basename_Source_Maps.Map;
      --  Visible sources by basename
      Src_Infos       : Src_Info_Maps.Map;
      --  Sources identified in the source directories of the view, indexed
      --  by their full names.
      Langs_Usage     : Sources_By_Langs_Maps.Map;
      --  Indicates the number of sources per language. Allows detecting
      --  unused languages.
      Actually_Excluded : Basename_Source_Maps.Map;
      --  Excluded files found during source loading, used by compiler mapping

      Own_CUs         : Unit_Maps.Map;
      --  The compilation units whose main unit belongs to the table's view

      Temp_Files      : GPR2.Containers.Filename_Map;
      --  Holds the path of temp files created during build

      Visible_Src_Cache     : Visible_Source_Maps.Map;
      Visible_Src_Cache_Gen : Natural := 0;
      --  Cache for Visible_Source, dropped when the tree's source
      --  generation changes.

      Runtime_Override_Units : Runtime_Override_Maps.Map;
      --  Visible runtime overrides by basename, shared by the view's binders.
      --  Rebuilt after source refreshes; read-only during action execution.

      Visible_Source_Closure : GPR2.Project.View.Vector.Object;
      --  The list of views to consider during source lookup: the current view
      --  and its dependencies recursively, together with the extended
      --  view's dependencies if any.
      --  Set at creation only: updating it in place would need
      --  Visible_Src_Cache to be cleared.

      Interface_Closure          : GPR2.Build.Compilation_Unit.Maps.Map;
      Interface_Closure_Computed : Boolean := False;
      --  Cache for GPR2.Project.View.Interface_Closure, which is costly to
      --  compute (in particular for aggregate libraries) and is queried once
      --  per action during the build graph population. Reset whenever the
      --  sources of the tree are refreshed.

      case Is_Root is
         when True =>
            CUs       : Compilation_Unit_Maps.Map;
            --  List of compilation units, indexed by their identifier
            --
            --  Note: Compilation units are stored in root projects: for
            --  aggregate projects, each root project has its own list
            --  to prevent clashes between independant subtrees.

            Runtime_Override_Files : Containers.Filename_Set;
            --  Original and replacement basenames of runtime overrides.
            --  Visibility is checked when caching unit names.

            Separates : Name_Maps.Map;
            --  Map of separates to their declaring unit. Note that the
            --  parent unit may not be a compilation unit but another separate

         when False =>
            null;
      end case;
   end record;

   package Data_Refs is new GNATCOLL.Refcount.Shared_Pointers (View_Data);

   subtype View_Data_Ref is Data_Refs.Reference_Type;

   procedure Load_Sources
     (Data           : View_Data_Ref;
      Messages       : in out GPR2.Log.Object;
      Matching_Units : Boolean := True);
   --  Load and validate sources, recursively loading extended views

   procedure Reset_Sources (Data : View_Data_Ref);
   --  Clear source tables before a full refresh, retaining the view DB.

   procedure Resolve_Separates
     (Data     : View_Data_Ref;
      Messages : in out GPR2.Log.Object);
   --  Resolve nested separates after all units have been registered.

   procedure Update_Runtime_Overrides (Data : View_Data_Ref);
   --  Rebuild runtime dependency lookup after all affected views are refreshed

   View_Base_For : access function
                     (Data : View_Data) return View_Db.Object;
   Get_Ref       : access function
                     (Obj : View_Db.Object) return View_Data_Ref;

   function Source
     (Data     : View_Data_Ref;
      Basename : Simple_Name) return Build.Source.Object;

   function Source
     (Data  : View_Data_Ref;
      Proxy : Source_Proxy) return Build.Source.Object
     with Inline;

   procedure Source_Status
     (Data          : View_Data_Ref;
      Proxy         : Source_Proxy;
      Is_Visible    : out Boolean;
      Is_Compilable : out Boolean);
   --  Check source status without copying its units and dependencies

   function Unit_Dependencies
     (Data     : View_Data_Ref;
      Basename : Simple_Name;
      Index    : Unit_Index) return GPR2.Containers.Name_Set;
   --  The dependencies of that unit. Unlike Source above, does not build a
   --  Source object, which would copy the source and all its units.

   function Visible_Source
     (Data      : View_Data_Ref;
      Basename  : Simple_Name;
      Ambiguous : out Boolean) return Build.Source.Object;

   function Visible_Source
     (Data : View_Data_Ref;
      Path : GPR2.Path_Name.Object) return Build.Source.Object;

   function Get_Data
     (Db   : access GPR2.Build.Tree_Db.Object;
      View : GPR2.Project.View.Object) return View_Data_Ref;

end GPR2.Build.View_Tables;
