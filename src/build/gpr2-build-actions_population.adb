--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Vectors;

with GNATCOLL.Directed_Graph;
with GNATCOLL.Traces;

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Actions.Archive_Table_List;
with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Actions.Link;
with GPR2.Build.Actions.Link_Options_Insert;
with GPR2.Build.Actions.Link.Partial;
with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Actions.Sets;
with GPR2.Build.Artifacts;
with GPR2.Build.Artifacts.Library;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.Object_File;
pragma Warnings (Off);
with GPR2.Build.Compilation_Unit.Maps;
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Build.Tree_Db;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.View.Set;
with GPR2.Project.View.Vector;
with GPR2.Source_Reference;
with GPR2.View_Ids.Set;

package body GPR2.Build.Actions_Population is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create ("GPR.BUILD.ACTIONS_POPULATION",
                                      GNATCOLL.Traces.Off);

   --  Some notes on the initial DAG population:
   --  First step, population of libraries for the tree:
   --  - the output artifacts is created and added to the tree
   --  - proper link or archive operation is created for the libs
   --  - the list of objects that belong the lib is created
   --    * corresponding compilation actions are populatedw

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   type Link_Array is
     array (Positive range <>) of GPR2.Build.Actions.Link.Object;

   type Bind_Array is
     array (Positive range <>) of GPR2.Build.Actions.Ada_Bind.Object;

   package Library_Helper is

      type Object is tagged record
         View                : GPR2.Project.View.Object;
         Bind                : GPR2.Build.Actions.Ada_Bind.Object;
         Static_Libs_Deps    : GPR2.View_Ids.Set.Set;
         Shared_Libs_Deps    : GPR2.View_Ids.Set.Set;
         Link_Options_Insert : GPR2.Build.Actions.Link_Options_Insert.Object;
         Partial_Link        : GPR2.Build.Actions.Link.Partial.Object;
         Main_Link           : GPR2.Build.Actions.Link.Object;
      end record;

      function Initial_Link_Action
        (Self : in out Object) return GPR2.Build.Actions.Link.Object'Class;

      function Final_Link_Action
        (Self : Object) return GPR2.Build.Actions.Link.Object'Class;

   private

      function Initial_Link_Action
        (Self : in out Object) return GPR2.Build.Actions.Link.Object'Class
      is (if Self.Partial_Link.Is_Defined
          then Self.Partial_Link
          else Self.Main_Link);

      function Final_Link_Action
        (Self : Object) return GPR2.Build.Actions.Link.Object'Class
      is (Self.Main_Link);

   end Library_Helper;

   package LH renames Library_Helper;
   use type LH.Object;

   package View_Id_Library_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => View_Ids.View_Id,
      Element_Type => LH.Object,
      "<"          => View_Ids."<");
   package Library_Vector is new Ada.Containers.Vectors
     (Positive, LH.Object);

   package Library_Map is
      function Order_Libs
        (Static_Libs : View_Ids.Set.Set;
         Cache       : View_Id_Library_Map.Map;
         Has_Cycle   : out Boolean) return Library_Vector.Vector;
      --  This tries to perform a topological sort of the static libraries
      --  given in Static_Libs according to the library map computed with the
      --  action DAG population. If unsolvable (cycle in the dependencies),
      --  then Has_Cycle is set and a flat list is returned.

      function Shortest_Cycle
        (Static_Libs : View_Ids.Set.Set;
         Cache       : View_Id_Library_Map.Map) return Library_Vector.Vector;
      --  In case the call to Order_Libs above sets the Has_Cycle parameter,
      --  this function will show the shortest cycle found in the library
      --  dependencies.

   private

      use GNATCOLL.Directed_Graph;

      package View_Node_Map is new Ada.Containers.Ordered_Maps
        (Key_Type     => View_Ids.View_Id,
         Element_Type => GNATCOLL.Directed_Graph.Node_Id,
         "<"          => View_Ids."<",
         "="          => GNATCOLL.Directed_Graph."=");
      package Node_View_Map is new Ada.Containers.Ordered_Maps
        (Key_Type     => GNATCOLL.Directed_Graph.Node_Id,
         Element_Type => View_Ids.View_Id,
         "<"          => GNATCOLL.Directed_Graph."<",
         "="          => View_Ids."=");

      type Map is tagged limited record
         DAG     : Directed_Graph;
         To_Node : View_Node_Map.Map;
         To_View : Node_View_Map.Map;
         Values  : View_Id_Library_Map.Map;
      end record;

      function Contains
        (Self : Map; Key : GPR2.Project.View.Object) return Boolean
      is (Self.Values.Contains (Key.Id));

      procedure Include
        (Self    : in out Map;
         Element : LH.Object);

      procedure Add_Successor
        (Self      : in out Map;
         Key       : GPR2.Project.View.Object;
         Successor : GPR2.Project.View.Object);

      procedure Replace
        (Self    : in out Map;
         Element : LH.Object);

      function Serialize
        (Self       : in out Map;
         Has_Circle :    out Boolean) return Library_Vector.Vector;
   end Library_Map;

   function Populate_Library
     (Tree_Db               : GPR2.Build.Tree_Db.Object_Access;
      View                  : GPR2.Project.View.Object;
      Options               : Build.Options.Build_Options;
      Cache                 : in out View_Id_Library_Map.Map;
      SAL_Closure           : in out Boolean;
      With_Externally_Built : Boolean) return Boolean;
   --  If previous is set, it indicates the previously withed lib for the
   --  view that populates its library dependencies. This is used to keep the
   --  proper topological order of the withed libraries (and thus proper
   --  symbol resolutions)

   function As_Unit_Location
     (Basename       : Value_Type;
      Index          : Unit_Index;
      View           : GPR2.Project.View.Object;
      Options        : Build.Options.Build_Options;
      Error_Reported : out Boolean)
      return Compilation_Unit.Unit_Location_Vector;

   function Populate_All
     (Tree_Db               : GPR2.Build.Tree_Db.Object_Access;
      View                  : GPR2.Project.View.Object;
      Single_View           : Boolean;
      Options               : Build.Options.Build_Options;
      With_Externally_Built : Boolean) return Boolean;

   function Populate_Mains
     (Tree_Db               : GPR2.Build.Tree_Db.Object_Access;
      View                  : GPR2.Project.View.Object;
      Mains                 : GPR2.Build.Compilation_Unit.Unit_Location_Vector;
      Options               : Build.Options.Build_Options;
      With_Externally_Built : Boolean) return Boolean;

   function Populate_Withed_Projects
     (Tree_Db               : GPR2.Build.Tree_Db.Object_Access;
      Options               : Build.Options.Build_Options;
      Closure               : in out GPR2.Project.View.Set.Object;
      Cache                 : in out View_Id_Library_Map.Map;
      Static_Lib_Closure    : out View_Ids.Set.Set;
      Shared_Lib_Closure    : out View_Ids.Set.Set;
      Has_SAL               : in out Boolean;
      With_Externally_Built : Boolean) return Boolean;
   --  Handle the population of withed projects
   --  Closure will contain the list of withed standard views
   --  Libs is the list of withed libraries

   package body Library_Map is

      procedure Initialize
        (Self : out Map;
         Static_Libs : View_Ids.Set.Set;
         Cache       : View_Id_Library_Map.Map);

      -------------------
      -- Add_Successor --
      -------------------

      procedure Add_Successor
        (Self      : in out Map;
         Key       : GPR2.Project.View.Object;
         Successor : GPR2.Project.View.Object)
      is
         From : constant Node_Id := Self.To_Node.Element (Key.Id);
         Succ : constant Node_Id := Self.To_Node.Element (Successor.Id);
      begin
         if Succ /= From then
            Self.DAG.Add_Predecessor (Succ, From);
         end if;
      end Add_Successor;

      -------------
      -- Include --
      -------------

      procedure Include
        (Self    : in out Map;
         Element : LH.Object)
      is
         New_Node : Node_Id;
      begin
         if not Self.Values.Contains (Element.View.Id) then
            New_Node := Self.DAG.Add_Node;
            Self.To_Node.Insert (Element.View.Id, New_Node);
            Self.To_View.Insert (New_Node, Element.View.Id);
            Self.Values.Insert (Element.View.Id, Element);
         end if;
      end Include;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self : out Map;
         Static_Libs : View_Ids.Set.Set;
         Cache       : View_Id_Library_Map.Map) is
      begin
         for Id of Static_Libs loop
            declare
               Lib : constant LH.Object :=
                       Cache.Element (Id);
               Sub : LH.Object;
            begin
               Self.Include (Lib);

               for V_Id of Lib.Static_Libs_Deps.Union
                 (Lib.Shared_Libs_Deps)
               loop
                  Sub := Cache.Element (V_Id);

                  Self.Include (Sub);
                  Self.Add_Successor (Lib.View, Sub.View);
               end loop;
            end;
         end loop;
      end Initialize;

      ----------------
      -- Order_Libs --
      ----------------

      function Order_Libs
        (Static_Libs : View_Ids.Set.Set;
         Cache       : View_Id_Library_Map.Map;
         Has_Cycle   : out Boolean) return Library_Vector.Vector
      is
         Self : Map;
      begin
         Self.Initialize (Static_Libs, Cache);

         return Self.Serialize (Has_Cycle);
      end Order_Libs;

      -------------
      -- Replace --
      -------------

      procedure Replace
        (Self    : in out Map;
         Element : LH.Object) is
      begin
         Self.Values.Replace (Element.View.Id, Element);
      end Replace;

      ---------------
      -- Serialize --
      ---------------

      function Serialize
        (Self       : in out Map;
         Has_Circle : out Boolean) return Library_Vector.Vector
      is
         Node : Node_Id;
      begin
         Has_Circle := False;

         return Result : Library_Vector.Vector do
            Self.DAG.Start_Iterator;

            while Self.DAG.Next (Node) loop
               Result.Append (Self.Values (Self.To_View (Node)));
            end loop;
         end return;

      exception
         when GNATCOLL.Directed_Graph.DG_Error =>
            Traces.Trace
              ("Circular dependency detected in the library dependencies, " &
                 "will use --start-group/--end-group");
            Has_Circle := True;

            return Result : Library_Vector.Vector do
               for Lib of Self.Values loop
                  Result.Append (Lib);
               end loop;
            end return;
      end Serialize;

      ---------------------
      -- Shortest_Circle --
      ---------------------

      function Shortest_Cycle
        (Static_Libs : View_Ids.Set.Set;
         Cache       : View_Id_Library_Map.Map) return Library_Vector.Vector
      is
         Self : Map;
      begin
         Self.Initialize (Static_Libs, Cache);

         return Result : Library_Vector.Vector do
            for Node of Self.DAG.Shortest_Cycle loop
               Result.Append (Cache (Self.To_View (Node)));
            end loop;
         end return;
      end Shortest_Cycle;
   end Library_Map;

   ----------------------
   -- As_Unit_Location --
   ----------------------

   function As_Unit_Location
     (Basename       : Value_Type;
      Index          : Unit_Index;
      View           : GPR2.Project.View.Object;
      Options        : Build.Options.Build_Options;
      Error_Reported : out Boolean)
      return Compilation_Unit.Unit_Location_Vector
   is
      use Compilation_Unit;
      use type GPR2.Path_Name.Object;

      Src       : GPR2.Build.Source.Object;
      Tree_Db   : constant GPR2.Build.Tree_Db.Object_Access :=
                    View.Tree.Artifacts_Database;
      Res       : Unit_Location_Vector;
      SN        : constant Simple_Name :=
                    Path_Name.Simple_Name (Filename_Type (Basename));
      Full      : Path_Name.Object;
      Ambiguous : Boolean := False;

   begin
      Error_Reported := False;

      if Filename_Optional (SN) /= Filename_Optional (Basename) then
         --  The parameter is not a simple name, so check for a relative
         --  path.
         Full := Path_Name.Create_File (Filename_Type (Basename));
         Src := View.Visible_Source (Full);

      else
         Src := View.Visible_Source (SN, Ambiguous);
      end if;

      if not Src.Is_Defined then
         for Driver of View.Attributes (PRA.Compiler.Driver) loop
            --  Check all languages that have a compiler driver
            declare
               Lang : constant Language_Id :=
                        +Name_Type
                          (GPR2.Project.Attribute.Index (Driver).Value);
            begin
               Src := View.Visible_Source
                 (View.Suffixed_Simple_Name (String (SN), Lang), Ambiguous);

               exit when Src.Is_Defined;

               --  Ada also accepts specs as main
               if Lang = Ada_Language then
                  Src := View.Visible_Source
                    (SN & Simple_Name
                       (View.Attribute
                            (PRA.Naming.Spec_Suffix,
                             PAI.Create (Ada_Language)).Value.Text),
                     Ambiguous);
               end if;
            end;

            exit when Src.Is_Defined;
         end loop;
      end if;

      if not Src.Is_Defined then
         return Compilation_Unit.Empty_Vector;
      end if;

      if Src.Is_Defined
        and then Filename_Type (SN) /= Filename_Type (Basename)
      then
         --  Input was not a simple_name but a relative path, check that we
         --  have the right source, otherwise this means the source is not
         --  visible.

         declare
            Path : constant Path_Name.Object :=
                     GPR2.Path_Name.Create_File (Filename_Type (Basename));

         begin
            if Path /= Src.Path_Name then
               return Compilation_Unit.Empty_Vector;
            end if;
         end;
      end if;

      if Ambiguous then
         Tree_Db.Reporter.Report
           (Message.Create
              (Message.Error,
               "multiple sources were found for: """ &
                 Basename & '"',
               Source_Reference.Create (View.Path_Name.Value, 0, 0)));
         Error_Reported := True;

         return Compilation_Unit.Empty_Vector;
      end if;

      if Index /= No_Index then
         if not Src.Has_Units then
            Tree_Db.Reporter.Report
              (Message.Create
                 (Message.Error,
                  "unit index specified with a non unit-based source",
                  Source_Reference.Create (Src.Path_Name.Value, 0, 0)));
            Error_Reported := True;

            return Compilation_Unit.Empty_Vector;

         elsif not Src.Has_Unit_At (Index) then
            Tree_Db.Reporter.Report
              (Message.Create
                 (Message.Error,
                  " no unit for the index" & Index'Image,
                  Source_Reference.Create (Src.Path_Name.Value, 0, 0)));
            Error_Reported := True;

            return Compilation_Unit.Empty_Vector;
         end if;
      end if;

      if Src.Owning_View.Is_Library
        and then not Options.Unique_Compilation
        and then not Options.Unique_Compilation_Recursive
        and then (not Options.Restricted_Build_Phase
                  or else Options.Bind_Phase_Mandated
                  or else Options.Link_Phase_Mandated)
      then
         Tree_Db.Reporter.Report
           (Message.Create
              (Message.Error,
               "main cannot be a source of a library project: """ &
                 Basename & '"',
               Source_Reference.Create
                 (Src.Owning_View.Path_Name.Value, 0, 0)));
         Error_Reported := True;

         return Compilation_Unit.Empty_Vector;
      end if;

      if Src.Has_Units
        and then not Src.Has_Single_Unit
        and then Index = No_Index
      then
         for U of Src.Units loop
            Res.Append
              (Unit_Location'(View   => Src.Owning_View,
                              Source => Src.Path_Name,
                              Index  => U.Index));
         end loop;

      else
         Res.Append
           (Unit_Location'(View   => Src.Owning_View,
                           Source => Src.Path_Name,
                           Index  => Index));
      end if;

      return Res;
   end As_Unit_Location;

   ----------------------
   -- Populate_Actions --
   ----------------------

   function Populate_Actions
     (Tree                  : GPR2.Project.Tree.Object;
      Options               : GPR2.Build.Options.Build_Options;
      Static_Actions        : Boolean;
      With_Externally_Built : Boolean := False;
      Populate_Mains_Only   : Boolean := False) return Boolean

   is
      Tree_Db     : GPR2.Build.Tree_Db.Object_Access renames
                      Tree.Artifacts_Database;
      Result      : Boolean := True;
      Visited     : View_Ids.Set.Set;
      Pos         : View_Ids.Set.Cursor;
      Inserted    : Boolean;
      Cache       : View_Id_Library_Map.Map;
      Static_Libs : View_Ids.Set.Set;
      Shared_Libs : View_Ids.Set.Set;
      Src         : GPR2.Build.Source.Object;
      Mains       : GPR2.Build.Compilation_Unit.Unit_Location_Vector;
      To_Remove   : Actions.Sets.Set;
      Has_Error   : Boolean;
      Has_SAL     : Boolean := False;
      Closure     : GPR2.Project.View.Set.Object;
      use type Ada.Containers.Count_Type;

   begin
      Tree_Db.Set_Build_Options (Options);

      --  Lookup the source(s) given explicitly on the command line, if any

      Inserted := False;

      Mains := Resolve_Mains
        (Tree, Options, Has_Error);

      if Has_Error then
         return False;
      end if;

      --  Check if we need to generate the mapping file for mains, and perform
      --  verifications that all parameters are correct in the given context

      if Options.Create_Map_File then
         declare
            Attr           : constant GPR2.Project.Attribute.Object :=
                               Tree.Configuration.Corresponding_View.Attribute
                                 (PRA.Linker.Map_File_Option);
            Multiple_Mains : Boolean := False;
         begin
            --  Check if there's support from the linker, and then check that
            --  we have a main to link

            if not Attr.Is_Defined or else Attr.Values.Is_Empty then
               pragma Annotate (Xcov, Exempt_On, "defensive code");
               Tree_Db.Reporter.Report
                 ("error: selected linker does not allow creating a map file",
                  To_Stderr => True,
                  Level     => GPR2.Message.Important);
               return False;
               pragma Annotate (Xcov, Exempt_Off);

            elsif Options.Mapping_File_Name /= Null_Unbounded_String then
               if Mains.Length > 1 then
                  Multiple_Mains := True;

               elsif Mains.Length = 0 then
                  for V of Tree.Namespace_Root_Projects loop
                     if V.Has_Mains and then V.Mains.Length > 1 then
                        Multiple_Mains := True;
                     end if;
                  end loop;
               end if;

               if Multiple_Mains then
                  Tree_Db.Reporter.Report
                    ("error: map file name is specified while there are " &
                       "multiple mains",
                     To_Stderr => True,
                     Level     => GPR2.Message.Important);

                  return False;
               end if;
            end if;
         end;
      end if;

      for V of Tree.Namespace_Root_Projects loop
         Visited.Insert (V.Id, Pos, Inserted);

         if Inserted then
            if Options.Unique_Compilation
              or else Options.Unique_Compilation_Recursive
            then
               -----------------------
               --  Handle -u and -U --
               -----------------------

               if Mains.Is_Empty then
                  if not Populate_Mains_Only then
                  --  compile all sources, recursively in case -U is set
                     if Options.Unique_Compilation then
                        Result := Populate_All
                        (Tree_Db, V, True, Options, With_Externally_Built);

                     else
                        for C of V.Closure (True, False, True) loop
                           Result := Populate_All
                           (Tree_Db, C, True, Options, With_Externally_Built);
                           exit when not Result;
                        end loop;
                     end if;
                  end if;
               else
                  --  Only compile the given sources

                  for M of Mains loop
                     Src := V.Visible_Source (M.Source);

                     --  Src may not be part of the current subtree

                     if Src.Is_Defined then
                        if Src.Language = Ada_Language then
                           declare
                              Comp : GPR2.Build.Actions.Compile.Ada.Object;
                           begin
                              Comp.Initialize
                                (V.Unit (Src.Unit (M.Index).Name));

                              if not Tree_Db.Add_Action (Comp) then
                                 return False;
                              end if;
                           end;

                        else
                           declare
                              Comp : GPR2.Build.Actions.Compile.Object;
                           begin
                              Comp.Initialize (Src);

                              if not Tree_Db.Add_Action (Comp) then
                                 return False;
                              end if;
                           end;
                        end if;
                     end if;
                  end loop;
               end if;

            else
               --------------------------
               --  Handle general case --
               --------------------------

               case V.Kind is
                  when K_Standard | K_Abstract =>
                     if V.Has_Mains or else not Mains.Is_Empty then
                        Result := Populate_Mains
                          (Tree_Db, V, Mains, Options, With_Externally_Built);
                     elsif not Populate_Mains_Only then
                        Result := Populate_All
                          (Tree_Db, V, False, Options, With_Externally_Built);
                     end if;

                  when K_Library | K_Aggregate_Library =>
                     if not Populate_Mains_Only then
                        Result :=
                          Populate_Library
                            (Tree_Db, V, Options, Cache, Has_SAL,
                             With_Externally_Built);
                     end if;

                  when others =>
                     if not Populate_Mains_Only then

                        Closure.Include (V);
                        Result := Populate_Withed_Projects
                        (Tree_Db, Options, Closure, Cache,
                           Static_Libs, Shared_Libs, Has_SAL,
                           With_Externally_Built);
                     end if;
               end case;
            end if;
         end if;

         if not Result then
            return False;
         end if;
      end loop;

      if not Options.Unique_Compilation
        and then not Options.Unique_Compilation_Recursive
      then
         Result := Tree_Db.Propagate_Actions;
      end if;

      if Options.Restricted_Build_Phase then
         for A of Tree_Db.All_Actions loop
            declare
               Compile_Phase_En : Boolean;
               Bind_Phase_En    : Boolean;
               Link_Phase_En    : Boolean;
            begin
               --  Handling of -c -b and -l is a bit tricky for gpr2 since you
               --  need to remember how gpr1 was structured. Binding of mains
               --  was done by gprbind while links were done for libs by
               --  gprlib, while the (optional) binding phase for libs is
               --  thus just hidden so do not apply to those switches, and link
               --  actually done by gprbuild was only for mains..
               --
               --  So basically -c is pretty well respected, because the old
               --  gprbuild had full control over this phase.
               --
               --  -b only used to control calls to gprbind, so is only valid
               --  if the action is a bind action for an executable, but not
               --  for a library.
               --
               --  -l is only applicable to executables, so not for linking
               --  libs.
               --
               --  ??? for testsuite reasons we try to keep here the same
               --  reasoning, but this set of switches don't really make sense
               --  here anymore and could be simplified to simply filter the
               --  actions according to their classes.

               Compile_Phase_En := Options.Compile_Phase_Mandated
                 and then A in Actions.Compile.Object'Class;

               Bind_Phase_En := Options.Bind_Phase_Mandated
                 and then
                   (A in Actions.Ada_Bind.Object'Class
                    or else A in Actions.Post_Bind.Object'Class
                    or else (A in Actions.Link.Object'Class
                             and then Actions.Link.Object (A).Is_Library));

               Link_Phase_En := Options.Link_Phase_Mandated
                 and then A in Actions.Link.Object'Class
                 and then not Actions.Link.Object (A).Is_Library;

               if not
                 (Compile_Phase_En
                  or else Bind_Phase_En
                  or else Link_Phase_En)
               then
                  To_Remove.Include (A);
               end if;
            end;
         end loop;
      end if;

      if not Options.Restricted_To_Languages.Is_Empty then
         for A of Tree_Db.All_Actions loop
            if A in Actions.Compile.Object'Class
              and then not Options.Restricted_To_Languages.Contains
                (Actions.Compile.Object'Class (A).Language)
            then
               To_Remove.Include (A);
            end if;
         end loop;
      end if;

      for A of To_Remove loop
         Tree_Db.Action_Id_To_Reference (A.UID).Deactivate;
      end loop;

      if Static_Actions then
         --  This action tree is amended when executed by the Action's post
         --  command. In the case the tree is not executed, but need a complete
         --  tree, we thus need to manually iterate on the actions to allow
         --  them to perform this post-processing.

         declare
            New_Actions  : GPR2.Build.Actions.Action_Id_Sets.Set;
            Done_Actions : GPR2.Build.Actions.Action_Id_Sets.Set;
            List         : constant Build.Tree_Db.Actions_List'Class :=
                             Tree_Db.All_Actions;
         begin
            loop
               New_Actions.Clear;

               for C in List.Action_Iterate loop
                  declare
                     Action : constant Build.Actions.Object'Class :=
                                Build.Tree_Db.Element (C);
                  begin
                     if not Done_Actions.Contains (Action.UID) then
                        Done_Actions.Include (Action.UID);
                        New_Actions.Include (Action.UID);
                     end if;
                  end;
               end loop;

               exit when New_Actions.Is_Empty;

               for UID of New_Actions loop
                  declare
                     Action : Build.Actions.Object'Class :=
                                Tree_Db.Action (UID);
                  begin
                     if not Action.On_Ready_State then
                        return False;
                     end if;

                     Tree_Db.Action_Id_To_Reference (UID) := Action;
                  end;
               end loop;
            end loop;
         end;
      end if;

      return Result;
   end Populate_Actions;

   ------------------
   -- Populate_All --
   ------------------

   function Populate_All
     (Tree_Db               : GPR2.Build.Tree_Db.Object_Access;
      View                  : GPR2.Project.View.Object;
      Single_View           : Boolean;
      Options               : Build.Options.Build_Options;
      With_Externally_Built : Boolean) return Boolean
   is
      Closure     : GPR2.Project.View.Set.Object;
      Cache       : View_Id_Library_Map.Map;
      Static_Libs : View_Ids.Set.Set;
      Shared_Libs : View_Ids.Set.Set;
      Has_SAL     : Boolean := False;
   begin
      if View.Is_Externally_Built and then not With_Externally_Built then
         return True;
      end if;

      Closure.Include (View);

      if not Single_View
        and then not Populate_Withed_Projects
          (Tree_Db, Options, Closure, Cache, Static_Libs, Shared_Libs, Has_SAL,
           With_Externally_Built)
      then
         return False;
      end if;

      for V of Closure loop
         if not V.Is_Externally_Built or else With_Externally_Built then
            declare
               Comp : GPR2.Build.Actions.Compile.Ada.Object;
            begin
               for CU of V.Own_Units loop
                  Comp.Initialize (CU);

                  if not Tree_Db.Add_Action (Comp) then
                     return False;
                  end if;
               end loop;
            end;

            declare
               Comp : GPR2.Build.Actions.Compile.Object;
            begin
               for Src of V.Sources loop
                  if not Src.Has_Units
                    and then Src.Is_Compilable
                    and then Src.Kind = S_Body
                  then
                     Comp.Initialize (Src);

                     if not Tree_Db.Add_Action (Comp)
                     then
                        return False;
                     end if;
                  end if;
               end loop;
            end;
         end if;
      end loop;

      return True;
   end Populate_All;

   ----------------------
   -- Populate_Library --
   ----------------------

   function Populate_Library
     (Tree_Db               : GPR2.Build.Tree_Db.Object_Access;
      View                  : GPR2.Project.View.Object;
      Options               : Build.Options.Build_Options;
      Cache                 : in out View_Id_Library_Map.Map;
      SAL_Closure           : in out Boolean;
      With_Externally_Built : Boolean) return Boolean
   is
      Self           : LH.Object;
      Closure        : GPR2.Project.View.Set.Object;
      Static_Libs    : View_Ids.Set.Set;
      Shared_Libs    : View_Ids.Set.Set;
      Has_SAL        : Boolean := False;
      Partial_Linker : constant GPR2.Project.Attribute.Object :=
                         View.Attribute (PRA.Library_Partial_Linker);
      Response_File  : constant GPR2.Project.Attribute.Object :=
                         View.Attribute (PRA.Linker.Response_File_Format);
      use type Ada.Containers.Count_Type;

      Requires_Partial_Linking : constant Boolean :=
        View.Is_Library_Standalone
        and then (View.Is_Static_Library
                  or else (Response_File.Is_Defined
                           and then Ada.Characters.Handling.To_Lower
                             (Response_File.Value.Text) = "none"))
        and then Partial_Linker.Is_Defined
        and then (Partial_Linker.Values.Length > 0
                  and then Partial_Linker.Values.First_Element.Text /= "");

      Requires_Binding : constant Boolean :=
        View.Is_Library_Standalone
        and then (View.Language_Ids.Contains (Ada_Language)
                  or else (View.Kind = K_Aggregate_Library
                           and then (for some Agg of View.Aggregated =>
                                       Agg.Language_Ids.Contains
                                         (Ada_Language))));

      Sorted_Libs : Library_Vector.Vector;
      Has_Cycle   : Boolean;

      use GPR2.Project;

   begin
      if View.Is_Extended or else Cache.Contains (View.Id) then
         --  Extended library projects won't produce any library, so skip
         --  them. Also skip already analyzed library projects.

         return True;
      end if;

      Self.View := View;

      Self.Main_Link.Initialize
        (Kind     => Actions.Link.Library,
         Context  => View,
         No_Rpath => Options.No_Run_Path);

      if not Tree_Db.Add_Action (Self.Main_Link) then
         return False;
      end if;

      if Requires_Partial_Linking then
         Self.Partial_Link.Initialize (View);

         if not Tree_Db.Add_Action (Self.Partial_Link) then
            return False;
         end if;

         Tree_Db.Add_Input
           (Self.Main_Link.UID, Self.Partial_Link.Output, True);
      end if;

      --  Add the lib now to prevent infinite recursion in case of
      --  circular dependencies (e.g. A withes B that limited_withes A)

      Cache.Include (View.Id, Self);

      --  Gather the list of standard view deps and ensure the libs are
      --  populated.

      if View.Kind /= K_Aggregate_Library then
         Closure.Include (View);
      else
         for V of View.Aggregated loop
            Closure.Include (V);
         end loop;
      end if;

      if not Populate_Withed_Projects
        (Tree_Db, Options, Closure, Cache, Static_Libs, Shared_Libs, Has_SAL,
         With_Externally_Built)
      then
         return False;
      end if;

      --  Keep track of library dependencies and store them

      for Id of Static_Libs loop
         declare
            Sub    : constant LH.Object := Cache.Element (Id);
            Encaps : constant Boolean :=
                       Sub.View.Is_Library and then
                           Sub.View.Library_Standalone = Encapsulated;
         begin
            if not Self.Static_Libs_Deps.Contains (Sub.View.Id) then
               Self.Static_Libs_Deps.Include (Sub.View.Id);

               if not Encaps then
                  --  Also add the sublib's library dependencies. Skip for
                  --  encapsulated library, as it already contains eerything.

                  Self.Static_Libs_Deps.Union (Sub.Static_Libs_Deps);
                  Self.Shared_Libs_Deps.Union (Sub.Shared_Libs_Deps);
               end if;
            end if;
         end;
      end loop;

      for Id of Shared_Libs loop
         declare
            Sub    : constant LH.Object := Cache.Element (Id);
            Encaps : constant Boolean :=
                       Sub.View.Is_Library and then
                           Sub.View.Library_Standalone = Encapsulated;
         begin
            if not Self.Shared_Libs_Deps.Contains (Sub.View.Id) then
               Self.Shared_Libs_Deps.Include (Sub.View.Id);

               if not Encaps then
                  --  Also add the sublib's shared library dependencies
                  Self.Shared_Libs_Deps.Union (Sub.Static_Libs_Deps);
               end if;
            end if;
         end;
      end loop;

      SAL_Closure :=
        SAL_Closure or else Has_SAL or else View.Is_Library_Standalone;

      if View.Is_Externally_Built and then not With_Externally_Built then
         --  Update the Library object in Libs and stop the processing.

         Cache.Replace (View.Id, Self);

         return True;
      end if;

      if Requires_Binding then
         --  Create the binder action that will create the file in charge of
         --  elaborating and finalizing the lib. Used for standalone libraries.

         Self.Bind.Initialize
           (Basename       => View.Library_Name,
            Context        => View,
            Main_Unit      => Compilation_Unit.Undefined,
            SAL_In_Closure => Has_SAL,
            Skip           => Options.No_SAL_Binding);

         if not Tree_Db.Add_Action (Self.Bind) then
            return False;
         end if;

         --  Used by the linker so it can find its bind action easily.

         Actions.Link.Object'Class
           (Tree_Db.Action_Id_To_Reference (Self.Initial_Link_Action.UID)
              .Element.all)
           .Set_Bind_Action (Self.Bind);

         Actions.Link.Object'Class
           (Tree_Db.Action_Id_To_Reference (Self.Final_Link_Action.UID)
              .Element.all)
           .Set_Bind_Action (Self.Bind);

         --  Save the linker options in the form of an object file with a
         --  gpr-specific text section so that they can be retrieved later on
         --  by the final link, when such linker option has been removed from
         --  the project file (such as after an installation).
         --  Note that this action is only added for static libraries, because
         --  object file insertion is not easily supported on Windows for
         --  DLLs, and because shared libraries are already linked
         --  with the correct options, unlike static libs.

         if View.Is_Static_Library then
            Self.Link_Options_Insert.Initialize
              (Object_File => Self.Bind.Post_Bind.Object_File, View => View);

            if not Tree_Db.Add_Action (Self.Link_Options_Insert) then
               return False;
            end if;

            --  The linker options object is added directly to the last link
            --  phase so is skipped by the partial link that may not pick it up
            --  since it is not referenced.

            Tree_Db.Add_Input
            (Self.Final_Link_Action.UID,
               Self.Link_Options_Insert.Output_Object_File,
               True);
         end if;

         Tree_Db.Add_Input
           (Self.Initial_Link_Action.UID,
            Self.Bind.Post_Bind.Object_File,
            True);

      else
         --  Non standalone libraries: add all Ada units

         if View.Kind = K_Aggregate_Library then
            for Agg of View.Aggregated loop
               for CU of Agg.Own_Units loop
                  declare
                     Comp : GPR2.Build.Actions.Compile.Ada.Object;
                  begin
                     Comp.Initialize (CU);

                     if not Tree_Db.Add_Action (Comp) then
                        return False;
                     end if;

                     if Comp.Object_File.Is_Defined then
                        Tree_Db.Add_Input
                          (Self.Initial_Link_Action.UID,
                           Comp.Object_File,
                           False);
                     end if;
                  end;
               end loop;
            end loop;

         else
            for CU of View.Own_Units loop
               declare
                  Comp : GPR2.Build.Actions.Compile.Ada.Object;
               begin
                  Comp.Initialize (CU);

                  if not Tree_Db.Add_Action (Comp) then
                     return False;
                  end if;

                  if Comp.Object_File.Is_Defined then
                     Tree_Db.Add_Input
                       (Self.Initial_Link_Action.UID, Comp.Object_File, False);
                  end if;
               end;
            end loop;
         end if;
      end if;

      --  Now add all non-Ada compilable sources, including the ones from
      --  the withed standard projects.

      for V of Closure loop
         for Src of V.Sources loop
            if not Src.Has_Units
              and then Src.Is_Compilable
              and then Src.Kind = S_Body
            then
               declare
                  Comp : GPR2.Build.Actions.Compile.Object;
               begin
                  Comp.Initialize (Src);

                  if not Tree_Db.Add_Action (Comp) then
                     return False;
                  end if;

                  if Comp.Object_File.Is_Defined then
                     Tree_Db.Add_Input
                       (Self.Initial_Link_Action.UID, Comp.Object_File, False);
                  end if;
               end;
            end if;
         end loop;
      end loop;

      Sorted_Libs :=
        Library_Map.Order_Libs
          (Self.Static_Libs_Deps.Union (Self.Shared_Libs_Deps),
           Cache,
           Has_Cycle);

      Self.Main_Link.Set_Has_Library_Dependency_Circle (Has_Cycle);

      for Lib of Sorted_Libs loop
         declare
            Sublib : constant LH.Object := Cache.Element (Lib.View.Id);
            Encaps : constant Boolean :=
                       Self.View.Library_Standalone = Encapsulated;
         begin
            --  Update the DAG to ensure that libraries required to link a
            --  shared lib are present during the final link

            if (not Self.Final_Link_Action.Is_Static_Library)
              or else Encaps
            then
               Tree_Db.Add_Input
                 (Self.Initial_Link_Action.UID,
                  Sublib.Final_Link_Action.Output,
                  Encaps);
            end if;

            --  Make sure the libraries dependencies are bounded before the
            --  binder is called, as it's the bind action that copies the ali
            --  files to the library directory.

            if Self.Bind.Is_Defined then
               Tree_Db.Add_Input
                 (Self.Bind.UID, Sublib.Final_Link_Action.Output, False);
            end if;
         end;
      end loop;

      --  Update the Library object in Libs

      Cache.Replace (View.Id, Self);

      return True;
   end Populate_Library;

   --------------------
   -- Populate_Mains --
   --------------------

   function Populate_Mains
     (Tree_Db               : GPR2.Build.Tree_Db.Object_Access;
      View                  : GPR2.Project.View.Object;
      Mains                 : GPR2.Build.Compilation_Unit.Unit_Location_Vector;
      Options               : Build.Options.Build_Options;
      With_Externally_Built : Boolean) return Boolean
   is
      use type GPR2.Path_Name.Object;
      use type Ada.Containers.Count_Type;

      A_Comp       : Actions.Compile.Ada.Object;
      Comp         : Actions.Compile.Object;
      Source       : GPR2.Build.Source.Object;
      Archive      : Actions.Link.Object;
      Actual_Mains : Compilation_Unit.Unit_Location_Vector;

   begin
      if Mains.Is_Empty then
         Actual_Mains := View.Mains;
      else
         Actual_Mains := Mains;
      end if;

      for Loc of Actual_Mains loop
         if Loc.View.Is_Library
           and then not Options.Unique_Compilation
           and then not Options.Unique_Compilation_Recursive
           and then (not Options.Restricted_Build_Phase
                     or else Options.Link_Phase_Mandated)
         then
            Tree_Db.Reporter.Report
              (Message.Create
                 (Message.Error,
                  "main cannot be a source of a library project: """ &
                    String (Loc.Source.Simple_Name) & '"',
                  Source_Reference.Create (Loc.View.Path_Name.Value, 0, 0)));
            return False;
         end if;
      end loop;

      if Actual_Mains.Length > 1
        and then Length (Options.Output_File) > 0
      then
         Tree_Db.Reporter.Report
           (GPR2.Message.Create
              (GPR2.Message.Error,
               "cannot specify an output filename when there are several " &
                 "mains.",
               Source_Reference.Create (View.Path_Name.Value, 0, 0)));
         return False;
      end if;

      declare
         Tree          : constant GPR2.Project.Tree.Object := View.Tree;
         Bind          : Bind_Array (1 .. Natural (Actual_Mains.Length));
         Link          : Link_Array (1 .. Natural (Actual_Mains.Length));
         Closure       : GPR2.Project.View.Set.Object;
         Libs_Cache    : View_Id_Library_Map.Map;
         Sorted_Libs   : Library_Vector.Vector;
         Has_Cycle     : Boolean;
         Static_Libs   : View_Ids.Set.Set;
         Shared_Libs   : View_Ids.Set.Set;
         Idx           : Natural := 1;
         Skip          : Boolean := False;
         Direct_Import : Boolean := False;
         Has_SAL       : Boolean := False;
      begin
         --  First check the dependencies and retrieve the libraries

         Closure.Include (View);

         if not Populate_Withed_Projects
           (Tree_Db, Options, Closure, Libs_Cache,
            Static_Libs, Shared_Libs, Has_SAL, With_Externally_Built)
         then
            return False;
         end if;

         --  Organize the library dependencies

         Sorted_Libs :=
           Library_Map.Order_Libs (Static_Libs, Libs_Cache, Has_Cycle);

         for Id of Shared_Libs loop
            Sorted_Libs.Append (Libs_Cache.Element (Id));
         end loop;

         --  Check if we have non-ada objects that will require an archive
         --  before the final link

         Non_Ada_Archive_Loop :
         for V of Closure loop
            --  Add the non-Ada objects as dependencies

            for Src of V.Sources loop
               Skip := False;
               Direct_Import := False;

               if Src.Has_Units
                 or else not Src.Is_Compilable
                 or else Src.Kind /= S_Body
               then
                  Skip := True;
               end if;

               for Main of Actual_Mains loop
                  if Src.Path_Name = Main.Source then
                     --  Don't include mains in the closure of another main
                     Skip := True;
                  end if;
               end loop;

               if not Skip then
                  declare
                     Attr : constant Project.Attribute.Object :=
                              Src.Owning_View.Attribute
                                (PRA.Linker.Unconditional_Linking,
                                 PAI.Create (Src.Language));
                  begin
                     Direct_Import := Name_Type (Attr.Value.Text) = "True";
                  end;
               end if;

               if not Skip then
                  if not Direct_Import then
                     --  Need to create an intermediate library so that
                     --  foreign objects can be ignored by the linker
                     --  if no symbol is used from them. Else the linker
                     --  uses all objects that are on the command line.

                     Archive.Initialize
                       (Kind     => Build.Actions.Link.Global_Archive,
                        Context  => View);

                     if not Tree_Db.Add_Action (Archive) then
                        return False;
                     end if;

                     exit Non_Ada_Archive_Loop;
                  end if;
               end if;
            end loop;
         end loop Non_Ada_Archive_Loop;

         --  Process the mains one by one

         for Main of Actual_Mains loop
            Source := Main.View.Visible_Source (Main.Source);

            Link (Idx).Initialize
              (Kind     => Build.Actions.Link.Executable,
               Src      => Main,
               No_Rpath => Options.No_Run_Path,
               Output   => -Options.Output_File);

            if Options.Create_Map_File then
               if Length (Options.Mapping_File_Name) > 0 then
                  Link (Idx).Set_Mapping_File
                    (Filename_Type (To_String (Options.Mapping_File_Name)));
               else
                  Link (Idx).Set_Mapping_File
                    (Filename_Type (String (Link (Idx).Output.Path.Base_Name))
                     & ".map");
               end if;
            end if;

            if not Tree_Db.Add_Action (Link (Idx)) then
               return False;
            end if;

            if Source.Language = Ada_Language then
               declare
                  Unit : constant Compilation_Unit.Object :=
                           Main.View.Own_Unit
                             (Source.Units.Element (Main.Index).Name);
               begin
                  A_Comp.Initialize (Unit);

                  if not Tree_Db.Add_Action (A_Comp) then
                     return False;
                  end if;

                  Bind (Idx).Initialize
                    (A_Comp.Local_Ali_File.Path.Base_Filename,
                     Main.View,
                     Main_Unit      => Unit,
                     SAL_In_Closure => Has_SAL);

                  if not Tree_Db.Add_Action (Bind (Idx)) then
                     return False;
                  end if;
               end;

               --  We must detect all units from the main view that overrides
               --  the runtime in order to properly discover other overriden
               --  dependencies to compile and link to them.
               declare
                  Units : constant Compilation_Unit.Maps.Map :=
                            Main.View.Own_Units
                              (Overridden_From_Runtime => True);
               begin
                  for U of Units loop
                     declare
                        R_Comp : Actions.Compile.Ada.Object;
                     begin
                        R_Comp.Initialize (U);

                        if not Tree_Db.Add_Action (R_Comp) then
                           return False;
                        end if;

                        --  Add the overriden unit dependency file to discover
                        --  other potential overriden dependencies.
                        Tree_Db.Add_Input
                          (Bind (Idx).UID, R_Comp.Local_Ali_File, True);

                        --  Add the resulting object file to the final link to
                        --  resolve their symbols before the runtime for proper
                        --  overriding.
                        Tree_Db.Add_Input
                          (Link (Idx).UID, R_Comp.Object_File, True);
                     end;
                  end loop;
               end;

               Tree_Db.Add_Input (Bind (Idx).UID, A_Comp.Local_Ali_File, True);
               Tree_Db.Add_Input (Link (Idx).UID, A_Comp.Object_File, True);
               Tree_Db.Add_Input
                 (Link (Idx).UID, Bind (Idx).Post_Bind.Object_File, True);

            else
               Comp.Initialize (Source);

               if not Tree_Db.Add_Action (Comp) then
                  return False;
               end if;

               Tree_Db.Add_Input (Link (Idx).UID, Comp.Object_File, True);

               if (for some Lib of Closure =>
                     Lib.Language_Ids.Contains (Ada_Language))
                 or else (for some Lib of Static_Libs =>
                            Tree.Get_View
                              (Lib).Language_Ids.Contains (Ada_Language)
                          and then
                            not Tree.Get_View
                              (Lib).Is_Library_Standalone)
                 or else not Main.View.Attributes (PRA.Roots).Is_Empty
               then
                  --  ??? We don't need a bind phase per non-Ada main, we just
                  --  need one for the view. We do that only to remain
                  --  compatible with what gpr1build does?

                  Bind (Idx).Initialize
                    (Source.Path_Name.Base_Filename,
                     View,
                     Main_Unit      => Compilation_Unit.Undefined,
                     SAL_In_Closure => Has_SAL);

                  for V of Closure loop
                     for CU of V.Own_Units loop
                        Bind (Idx).Add_Root_Unit (CU);
                     end loop;
                  end loop;

                  for Lib of Static_Libs loop
                     declare
                        V : constant Project.View.Object :=
                              Tree.Get_View (Lib);
                     begin
                        if not V.Is_Library_Standalone then
                           for CU of V.Own_Units loop
                              Bind (Idx).Add_Root_Unit (CU);
                           end loop;
                        end if;
                     end;
                  end loop;

                  if not Tree_Db.Add_Action (Bind (Idx)) then
                     return False;
                  end if;

                  Tree_Db.Add_Input
                    (Link (Idx).UID,
                     Bind (Idx).Post_Bind.Object_File,
                     True);
               end if;
            end if;

            if Bind (Idx).Is_Defined then
               --  Used by the linker so it can find its bind action easily.

               Actions.Link.Object'Class
                 (Tree_Db.Action_Id_To_Reference (Link (Idx).UID)
                  .Element.all)
                 .Set_Bind_Action (Bind (Idx));
            end if;

            --  Add library dependencies: we need a proper ordering in
            --  particular when linking with archives, so that if libA.a
            --  depends on symbols from libB.a then we have -lA -lB in this
            --  order on the link command line, so that the linker knows about
            --  libA's undefined symbols before looking into libB.

            declare

               procedure Add_Archive_Table_List_Action
                 (Lib      : LH.Object;
                  Link_Idx : Natural);

               ----------------------------
               -- Add_Archive_Table_List --
               ----------------------------

               procedure Add_Archive_Table_List_Action
                 (Lib      : LH.Object;
                  Link_Idx : Natural)
               is
                  Archive_Table_List : Actions.Archive_Table_List.Object;
               begin
                  Archive_Table_List.Initialize
                    (Artifacts.Library.Object (Lib.Final_Link_Action.Output),
                     Main.View);

                  if not Tree_Db.Add_Action (Archive_Table_List) then
                     return;
                  end if;

                  --  The "archive table list" action generates additional
                  --  actions during its post-command phase. One of these
                  --  actions is required by the main link action to ensure
                  --  proper command-line options. To guarantee that the
                  --  "archive table list" action executes before the main
                  --  link, its output is added as a dependency of the linker
                  --  in the graph.

                  Tree_Db.Add_Input
                    (Link (Link_Idx).UID,
                     Archive_Table_List.UID_Artifact,
                     True);
               end Add_Archive_Table_List_Action;
            begin
               if Archive.Is_Defined then
                  Tree_Db.Add_Input
                    (Link (Idx).UID, Archive.Output, True);
               end if;

               for Lib of Sorted_Libs loop
                  Tree_Db.Add_Input
                    (Link (Idx).UID, Lib.Final_Link_Action.Output, True);

                  --  Make sure the bind action is executed after the
                  --  libraries are linked, to have access to the ALI files
                  --  in the lib directory.

                  if Bind (Idx).Is_Defined then
                     Tree_Db.Add_Input
                       (Bind (Idx).UID, Lib.Final_Link_Action.Output, False);
                  end if;

                  --  For standalone static libraries, linker options must
                  --  be updated to ensure proper elaboration of the library.
                  --  There are two possible scenarios:
                  --  * If the library is not externally built, the linker
                  --    options can be directly retrieved from the associated
                  --    action.
                  --  * If the library is externally built, the linker
                  --    options are embedded within the library itself,
                  --    typically in a custom section of the object file
                  --    generated by the binder. An action is created
                  --    to extract this information, as demonstrated above.

                  if Lib.View.Is_Library_Standalone
                    and then Lib.View.Is_Static_Library
                    and then Lib.View.Is_Externally_Built
                  then
                     Add_Archive_Table_List_Action (Lib, Idx);
                  end if;
               end loop;

               if Has_Cycle then
                  declare
                     Msg   : Unbounded_String;
                     Prev  : LH.Object;
                     First : Boolean := True;
                  begin
                     Tree_Db.Reporter.Report
                       (GPR2.Message.Create
                          (GPR2.Message.Warning,
                           "circular library dependency detected",
                           GPR2.Source_Reference.Create
                             (View.Path_Name.Value, 0, 0)));

                     for Lib of
                       Library_Map.Shortest_Cycle (Static_Libs, Libs_Cache)
                     loop
                        if First then
                           First := False;
                        else
                           Msg := +Prev.View.Library_Filename.Simple_Name;
                           Append (Msg, " depends on ");
                           Append
                             (Msg,
                              String (Lib.View.Library_Filename.Simple_Name));

                           Tree_Db.Reporter.Report
                             (GPR2.Message.Create
                                (GPR2.Message.Warning,
                                 -Msg,
                                 GPR2.Source_Reference.Create
                                   (Prev.View.Path_Name.Value, 0, 0)));
                        end if;

                        Prev := Lib;
                     end loop;
                  end;

                  --  Make the linker action generate --start-group --end-group
                  --  to resolve recursively the symbols in the libraries.

                  Actions.Link.Set_Has_Library_Dependency_Circle
                    (Actions.Link.Object'Class
                       (Tree_Db.Action_Id_To_Reference
                            (Link (Idx).UID).Element.all),
                     True);
               end if;
            end;

            Idx := Idx + 1;
         end loop;

         --  Check non-Ada sources: we create an intermedidate library for
         --  those sources so that the final link only picks up the actually
         --  used objects. A direct link with an explicit list of objects
         --  would actually use all those objects.

         for V of Closure loop
            --  Add the non-Ada objects as dependencies

            for Src of V.Sources loop
               Skip := False;
               Direct_Import := False;

               if Src.Has_Units
                 or else not Src.Is_Compilable
                 or else Src.Kind /= S_Body
               then
                  Skip := True;
               end if;

               for Main of Actual_Mains loop
                  if Src.Path_Name = Main.Source then
                     --  Don't include mains in the closure of another main
                     Skip := True;
                  end if;
               end loop;

               if not Skip then
                  declare
                     Attr : constant Project.Attribute.Object :=
                              Src.Owning_View.Attribute
                                (PRA.Linker.Unconditional_Linking,
                                 PAI.Create (Src.Language));
                  begin
                     Direct_Import := Name_Type (Attr.Value.Text) = "True";
                  end;
               end if;

               if not Skip then
                  Comp.Initialize (Src);

                  if not Tree_Db.Add_Action (Comp) then
                     return False;
                  end if;

                  if Comp.Object_File.Is_Defined then
                     if Direct_Import then
                        for J in Link'Range loop
                           Tree_Db.Add_Input
                             (Link (J).UID, Comp.Object_File, True);
                        end loop;
                     else
                        Tree_Db.Add_Input
                          (Archive.UID, Comp.Object_File, True);
                     end if;
                  end if;
               end if;
            end loop;
         end loop;
      end;

      return True;
   end Populate_Mains;

   ------------------------------
   -- Populate_Withed_Projects --
   ------------------------------

   function Populate_Withed_Projects
     (Tree_Db               : GPR2.Build.Tree_Db.Object_Access;
      Options               : Build.Options.Build_Options;
      Closure               : in out GPR2.Project.View.Set.Object;
      Cache                 : in out View_Id_Library_Map.Map;
      Static_Lib_Closure    : out View_Ids.Set.Set;
      Shared_Lib_Closure    : out View_Ids.Set.Set;
      Has_SAL               : in out Boolean;
      With_Externally_Built : Boolean) return Boolean
   is
      procedure Add_Deps (V  : GPR2.Project.View.Object);

      Todo : GPR2.Project.View.Vector.Object;
      Seen : GPR2.Project.View.Set.Object;

      --------------
      -- Add_Deps --
      --------------

      procedure Add_Deps (V : GPR2.Project.View.Object) is
      begin
         for Imp of V.Imports.Union (V.Limited_Imports) loop
            if not Seen.Contains (Imp) and then not Imp.Is_Extended then
               Todo.Append (Imp);
            end if;
         end loop;

         if V.Is_Extending then
            declare
               Extending_Views : GPR2.Project.View.Set.Object := V.Extended;
               --  Contains the extended views. This list allows to process
               --  transitively the extended views.
               Ext             : GPR2.Project.View.Object;
               --  Current processed extended view
            begin
               while not Extending_Views.Is_Empty loop
                  Ext := Extending_Views.First_Element;
                  Extending_Views.Delete_First;

                  for Imp of Ext.Imports.Union (Ext.Limited_Imports) loop
                     if not Seen.Contains (Imp) and then not Imp.Is_Extended
                     then
                        Todo.Append (Imp);
                     end if;
                  end loop;

                  if Ext.Is_Extending then

                     --  Process transitively the extended views
                     Extending_Views.Union (Ext.Extended);
                  end if;
               end loop;
            end;
         end if;
      end Add_Deps;

      Current : GPR2.Project.View.Object;

   begin
      for V of Closure loop
         Add_Deps (V);
      end loop;

      while not Todo.Is_Empty loop
         Current := Todo.First_Element;
         Todo.Delete_First;
         Seen.Include (Current);

         if Current.Is_Library then
            if not Populate_Library
              (Tree_Db, Current, Options, Cache, Has_SAL,
               With_Externally_Built)
            then
               return False;
            end if;

            if Current.Is_Static_Library then
               Static_Lib_Closure.Include (Current.Id);
               Static_Lib_Closure.Union
                 (Cache.Element (Current.Id).Static_Libs_Deps);
               Shared_Lib_Closure.Union
                 (Cache.Element (Current.Id).Shared_Libs_Deps);

            else
               Shared_Lib_Closure.Include (Current.Id);
               Shared_Lib_Closure.Union
                 (Cache.Element (Current.Id).Shared_Libs_Deps);
            end if;

         elsif Current.Is_Abstract then
               Add_Deps (Current);

         elsif not Current.Is_Configuration
           and then not Current.Is_Runtime
         then
            Closure.Include (Current);
            Add_Deps (Current);
         end if;
      end loop;

      return True;
   end Populate_Withed_Projects;

   -------------------
   -- Resolve_Mains --
   -------------------

   function Resolve_Mains
     (Tree    : GPR2.Project.Tree.Object;
      Options : Build.Options.Build_Options;
      Error   : out Boolean)
      return GPR2.Build.Compilation_Unit.Unit_Location_Vector
   is
      Result   : Compilation_Unit.Unit_Location_Vector;
      Inserted : Boolean;
   begin
      Error := False;

      for Main of Options.Mains loop
         Inserted := False;

         NS_Loop :
         for V of Tree.Namespace_Root_Projects loop
            declare
               M : constant Compilation_Unit.Unit_Location_Vector :=
                     As_Unit_Location
                       (Main,
                        Options.Unit_Index,
                        V,
                        Options,
                        Error);
            begin
               if Error then
                  return Compilation_Unit.Empty_Vector;
               end if;

               if not M.Is_Empty then
                  Inserted := True;
                  Result.Append (M);
                  exit NS_Loop;
               end if;
            end;
         end loop NS_Loop;

         if not Inserted then
            if Options.Unique_Compilation
              or else Options.Unique_Compilation_Recursive
            then
               Tree.Reporter.Report
                 (Message.Create
                    (Message.Error,
                     '"' & Main &
                       """ was not found in the sources of any project",
                     Source_Reference.Create
                       (Tree.Root_Project.Path_Name.Value, 0, 0)));
            else
               Tree.Reporter.Report
                 (Message.Create
                    (Message.Error,
                     '"' & Main &
                       """ was not found in the project",
                     Source_Reference.Create
                       (Tree.Root_Project.Path_Name.Value, 0, 0)));
            end if;

            Error := True;

            return Compilation_Unit.Empty_Vector;
         end if;
      end loop;

      return Result;
   end Resolve_Mains;

end GPR2.Build.Actions_Population;
