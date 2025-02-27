--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Vectors;
with Ada.Strings.Fixed;

with GNATCOLL.Directed_Graph;

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Actions.Link;
with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Actions.Sets;
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

   type Library_Type is record
      View : GPR2.Project.View.Object;
      Link : GPR2.Build.Actions.Link.Object;
   end record;

   No_Library : constant Library_Type := (others => <>);

   package Library_Map is
      use GNATCOLL.Directed_Graph;
      use GPR2.View_Ids;

      package View_Node_Map is new Ada.Containers.Ordered_Maps
        (Key_Type     => View_Id,
         Element_Type => Node_Id);
      package Node_View_Map is new Ada.Containers.Ordered_Maps
        (Key_Type     => Node_Id,
         Element_Type => GPR2.View_Ids.View_Id);
      package View_Id_Library_Map is new Ada.Containers.Ordered_Maps
        (Key_Type     => View_Id,
         Element_Type => Library_Type);
      package Library_Vector is new Ada.Containers.Vectors
        (Positive, Library_Type);

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
         Key     : GPR2.Project.View.Object;
         Element : Library_Type);

      procedure Add_Successor
        (Self      : in out Map;
         Key       : GPR2.Project.View.Object;
         Successor : GPR2.Project.View.Object);

      procedure Replace
        (Self    : in out Map;
         Key     : GPR2.Project.View.Object;
         Element : Library_Type);

      function Serialize (Self : in out Map) return Library_Vector.Vector;
   end Library_Map;

   function Populate_Library
     (Tree_Db     : GPR2.Build.Tree_Db.Object_Access;
      View        : GPR2.Project.View.Object;
      Options     : Build.Options.Build_Options;
      Libs        : in out Library_Map.Map;
      SAL_Closure : in out Boolean) return Boolean;
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
     (Tree_Db     : GPR2.Build.Tree_Db.Object_Access;
      View        : GPR2.Project.View.Object;
      Single_View : Boolean;
      Options     : Build.Options.Build_Options) return Boolean;

   function Populate_Mains
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Mains   : GPR2.Build.Compilation_Unit.Unit_Location_Vector;
      Options : Build.Options.Build_Options) return Boolean;

   function Populate_Withed_Projects
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Options : Build.Options.Build_Options;
      Closure : in out GPR2.Project.View.Set.Object;
      Libs    : in out Library_Map.Map;
      Has_SAL : in out Boolean) return Boolean;
   --  Handle the population of withed projects
   --  Closure will contain the list of withed standard views
   --  Libs is the list of withed libraries

   function Populate_Withed_Projects
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Options : Build.Options.Build_Options;
      Closure : in out GPR2.Project.View.Set.Object;
      Ctxt    : Library_Type;
      Libs    : in out Library_Map.Map;
      Has_SAL : in out Boolean) return Boolean;

   function Populate_Withed_Projects_Internal
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Options : Build.Options.Build_Options;
      Closure : in out GPR2.Project.View.Set.Object;
      Ctxt    : Library_Type;
      Libs    : in out Library_Map.Map;
      Has_SAL : in out Boolean) return Boolean;

   package body Library_Map is

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
         Self.DAG.Add_Predecessor (Succ, From);
      end Add_Successor;

      -------------
      -- Include --
      -------------

      procedure Include
        (Self    : in out Map;
         Key     : GPR2.Project.View.Object;
         Element : Library_Type)
      is
         New_Node : constant Node_Id := Self.DAG.Add_Node;
      begin
         Self.To_Node.Insert (Key.Id, New_Node);
         Self.To_View.Insert (New_Node, Key.Id);
         Self.Values.Insert (Key.Id, Element);
      end Include;

      -------------
      -- Replace --
      -------------

      procedure Replace
        (Self    : in out Map;
         Key     : GPR2.Project.View.Object;
         Element : Library_Type) is
      begin
         Self.Values.Replace (Key.Id, Element);
      end Replace;

      ---------------
      -- Serialize --
      ---------------

      function Serialize (Self : in out Map) return Library_Vector.Vector
      is
         Node : Node_Id;
      begin
         return Result : Library_Vector.Vector do
            Self.DAG.Start_Iterator;

            while Self.DAG.Next (Node) loop
               Result.Append (Self.Values (Self.To_View (Node)));
            end loop;
         end return;
      end Serialize;

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

      elsif Options.Unique_Compilation
        or else Options.Unique_Compilation_Recursive
      then
         Src := View.Visible_Source (SN, Ambiguous);
      else
         Src := View.Source (SN);
      end if;

      if not Src.Is_Defined then
         for Lang of View.Language_Ids loop
            Src := View.Visible_Source
              (View.Suffixed_Simple_Name (String (SN), Lang), Ambiguous);

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
     (Tree    : GPR2.Project.Tree.Object;
      Options : Build.Options.Build_Options) return Boolean
   is
      Tree_Db     : GPR2.Build.Tree_Db.Object_Access renames
                      Tree.Artifacts_Database;
      Result      : Boolean := True;
      Visited     : View_Ids.Set.Set;
      Pos         : View_Ids.Set.Cursor;
      Inserted    : Boolean;
      Libs        : Library_Map.Map;
      Src         : GPR2.Build.Source.Object;
      Mains       : GPR2.Build.Compilation_Unit.Unit_Location_Vector;
      To_Remove   : Actions.Sets.Set;
      Has_Error   : Boolean;
      Has_SAL     : Boolean := False;
      Closure     : GPR2.Project.View.Set.Object;
      use type Ada.Containers.Count_Type;

   begin
      Tree_Db.Set_Build_Options (Options);

      --  Lookup the source(s) given explicitly on the command line, if any.

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
            Attr : constant GPR2.Project.Attribute.Object :=
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
                  --  compile all sources, recursively in case -U is set
                  if Options.Unique_Compilation then
                     Result := Populate_All (Tree_Db, V, True, Options);
                  else
                     for C of V.Closure (True, False, True) loop
                        if not C.Is_Externally_Built then
                           Result := Populate_All
                             (Tree_Db, C, True, Options);
                           exit when not Result;
                        end if;
                     end loop;
                  end if;

                  return Result;

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
                  when K_Standard =>
                     if V.Has_Mains or else not Mains.Is_Empty then
                        Result := Populate_Mains (Tree_Db, V, Mains, Options);
                     else
                        Result := Populate_All (Tree_Db, V, False, Options);
                     end if;

                  when K_Library | K_Aggregate_Library =>
                     Result :=
                       Populate_Library (Tree_Db, V, Options, Libs, Has_SAL);

                  when others =>
                     Closure.Include (V);
                     Result := Populate_Withed_Projects
                       (Tree_Db, Options, Closure, Libs, Has_SAL);
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
            if not (Options.Compile_Phase_Mandated
                    and then A in Actions.Compile.Object'Class)
              and then not
                (Options.Bind_Phase_Mandated
                 and then
                   (A in Actions.Ada_Bind.Object'Class
                    or else A in Actions.Post_Bind.Object'Class))
              and then not (Options.Link_Phase_Mandated
                            and then A in Actions.Link.Object'Class)
            then
               To_Remove.Include (A);
            end if;
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

      return Result;
   end Populate_Actions;

   ------------------
   -- Populate_All --
   ------------------

   function Populate_All
     (Tree_Db     : GPR2.Build.Tree_Db.Object_Access;
      View        : GPR2.Project.View.Object;
      Single_View : Boolean;
      Options     : Build.Options.Build_Options) return Boolean
   is
      Closure : GPR2.Project.View.Set.Object;
      Libs    : Library_Map.Map;
      Has_SAL : Boolean := False;
   begin
      if View.Is_Externally_Built then
         return True;
      end if;

      Closure.Include (View);

      if not Single_View
        and then not Populate_Withed_Projects
                       (Tree_Db, Options, Closure, Libs, Has_SAL)
      then
         return False;
      end if;

      for V of Closure loop
         if not V.Is_Externally_Built then
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
     (Tree_Db     : GPR2.Build.Tree_Db.Object_Access;
      View        : GPR2.Project.View.Object;
      Options     : Build.Options.Build_Options;
      Libs        : in out Library_Map.Map;
      SAL_Closure : in out Boolean) return Boolean
   is
      Self    : Library_Type;
      Closure : GPR2.Project.View.Set.Object;
      Has_SAL : Boolean := False;
      Bind    : GPR2.Build.Actions.Ada_Bind.Object;

   begin
      if Libs.Contains (View) then
         return True;
      end if;

      Self.View := View;
      Self.Link.Initialize_Library (View);

      if not Tree_Db.Add_Action (Self.Link) then
         return False;
      end if;

      --  Add the lib now to prevent infinite recursion in case of
      --  circular dependencies (e.g. A withes B that limited_withes A)

      Libs.Include (View, Self);

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
        (Tree_Db, Options, Closure, Self, Libs, Has_SAL)
      then
         return False;
      end if;

      SAL_Closure :=
        SAL_Closure or else Has_SAL or else View.Is_Library_Standalone;

      if View.Is_Externally_Built then
         return True;
      end if;

      if View.Is_Library_Standalone then
         --  Create the binder action that will create the file in charge of
         --  elaborating and finalizing the lib. Used for standalone libraries.

         Bind.Initialize
           (Basename       => View.Library_Name,
            Context        => View,
            Has_Main       => False,
            SAL_In_Closure => Has_SAL,
            Skip           => Options.No_SAL_Binding);

         if not Tree_Db.Add_Action (Bind) then
            return False;
         end if;

         Tree_Db.Add_Input
           (Self.Link.UID, Bind.Post_Bind.Object_File, True);

         --  Now gather the list of units that compose the interface

         for CU of View.Interface_Closure loop
            declare
               Comp : GPR2.Build.Actions.Compile.Ada.Object;
            begin
               Comp.Initialize (CU);

               if not Tree_Db.Add_Action (Comp) then
                  return False;
               end if;

               Tree_Db.Add_Input (Self.Link.UID, Comp.Object_File, False);
               Tree_Db.Add_Input (Bind.UID, Comp.Local_Ali_File, True);
            end;
         end loop;

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

                     Tree_Db.Add_Input
                       (Self.Link.UID, Comp.Object_File, False);
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

                  Tree_Db.Add_Input (Self.Link.UID, Comp.Object_File, False);
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

                  Tree_Db.Add_Input (Self.Link.UID, Comp.Object_File, False);
               end;
            end if;
         end loop;
      end loop;

      --  If the current library depends on other libraries, add a dependency
      --  on the library output to avoid directly depending on the objects of
      --  the dependent libraries.

      declare
         Imported_Lib_Link : Actions.Link.Object;
      begin
         for Import of Self.View.Imports loop
            if Import.Is_Library then
               Imported_Lib_Link.Initialize_Library (Import);

               if not Tree_Db.Has_Action (Imported_Lib_Link.UID) then
                  return False;
               end if;

               Tree_Db.Add_Input
                 (Self.Link.UID, Imported_Lib_Link.Output, True);
            end if;
         end loop;
      end;

      --  Update the Library object in Libs
      Libs.Replace (View, Self);

      return True;
   end Populate_Library;

   --------------------
   -- Populate_Mains --
   --------------------

   function Populate_Mains
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Mains   : GPR2.Build.Compilation_Unit.Unit_Location_Vector;
      Options : Build.Options.Build_Options) return Boolean
   is
      A_Comp     : Actions.Compile.Ada.Object;
      Comp       : Actions.Compile.Object;
      Source     : GPR2.Build.Source.Object;
      Archive    : Actions.Link.Object;
      Actual_Mains : Compilation_Unit.Unit_Location_Vector;

      use type GPR2.Path_Name.Object;
      use type Ada.Containers.Count_Type;

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
         Bind      : Bind_Array (1 .. Natural (Actual_Mains.Length));
         Link      : Link_Array (1 .. Natural (Actual_Mains.Length));
         Attr      : GPR2.Project.Attribute.Object;
         Closure   : GPR2.Project.View.Set.Object;
         Libs      : Library_Map.Map;
         Idx       : Natural := 1;
         Skip      : Boolean := False;
         Has_Ada   : Boolean := False;
         Has_Other : Boolean := False;
         Has_SAL   : Boolean := False;
      begin
         --  First check the dependencies and retrieve the libraries

         Closure.Include (View);

         if not Populate_Withed_Projects
           (Tree_Db, Options, Closure, Libs, Has_SAL)
         then
            return False;
         end if;

         Closure_Loop :
         for V of Closure loop
            for L of V.Language_Ids loop
               if L = Ada_Language then
                  Has_Ada := True;
               elsif V.Is_Compilable (L) then
                  Has_Other := True;
               end if;

               exit Closure_Loop when Has_Ada and then Has_Other;
            end loop;
         end loop Closure_Loop;

         --  Process the mains one by one

         for Main of Actual_Mains loop
            Source := Main.View.Visible_Source (Main.Source);

            Link (Idx).Initialize_Executable
              (Main,
               -Options.Output_File);

            if Options.Create_Map_File then
               Attr := Main.View.Attribute (PRA.Linker.Map_File_Option);

               --  ??? TODO: Add a primitive to the link object to move the
               --  below processing there.

               if Length (Options.Mapping_File_Name) > 0 then
                  Link (Idx).Add_Option
                    (Attr.Value.Text & To_String (Options.Mapping_File_Name));
               else
                  Link (Idx).Add_Option
                    (Attr.Value.Text &
                       String (Link (Idx).Output.Path.Base_Name) & ".map");
               end if;
            end if;

            if not Tree_Db.Add_Action (Link (Idx)) then
               return False;
            end if;

            if Source.Language = Ada_Language then
               A_Comp.Initialize
                 (Main.View.Own_Unit (Source.Units.Element (Main.Index).Name));

               if not Tree_Db.Add_Action (A_Comp) then
                  return False;
               end if;

               Bind (Idx).Initialize
                 (A_Comp.Local_Ali_File.Path.Base_Filename,
                  Main.View,
                  Has_Main       => True,
                  SAL_In_Closure => Has_SAL);

               if not Tree_Db.Add_Action (Bind (Idx)) then
                  return False;
               end if;

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

               if Has_Ada then
                  --  ??? We don't need a bind phase per non-Ada main, we just
                  --  need one for the view. We do that only to remain
                  --  compatible with what gpr1build does?

                  Bind (Idx).Initialize
                    (Source.Path_Name.Base_Filename,
                     View,
                     Has_Main       => False,
                     SAL_In_Closure => Has_SAL);

                  if not Tree_Db.Add_Action (Bind (Idx)) then
                     return False;
                  end if;

                  for V of Closure loop
                     for CU of V.Own_Units loop
                        A_Comp.Initialize (CU);

                        if not Tree_Db.Add_Action (A_Comp) then
                           return False;
                        end if;

                        Tree_Db.Add_Input
                          (Bind (Idx).UID, A_Comp.Local_Ali_File, True);
                        Tree_Db.Add_Input
                          (Link (Idx).UID, A_Comp.Object_File, True);
                     end loop;
                  end loop;

                  Tree_Db.Add_Input
                    (Link (Idx).UID,
                     Bind (Idx).Post_Bind.Object_File,
                     True);
               end if;

               --  Hendle roots if any
               declare
                  procedure Add_CU (CU : Build.Compilation_Unit.Object);

                  ------------
                  -- Add_CU --
                  ------------

                  procedure Add_CU (CU : Build.Compilation_Unit.Object) is
                     Ada_Comp : GPR2.Build.Actions.Compile.Ada.Object;
                  begin
                     if CU.Is_Defined then
                        Ada_Comp.Initialize (CU);
                        Tree_Db.Add_Input
                          (Bind (Idx).UID,
                           Ada_Comp.Local_Ali_File,
                           True);

                        if Ada_Comp.Object_File.Is_Defined then
                           Tree_Db.Add_Input
                             (Link (Idx).UID,
                              Ada_Comp.Object_File,
                              True);
                        end if;
                     end if;
                  end Add_CU;

                  Attr     : constant GPR2.Project.Attribute.Object :=
                               View.Attribute
                                 (PRA.Roots,
                                  PAI.Create_Source (Main.Source.Simple_Name));
                  CU       : GPR2.Build.Compilation_Unit.Object;

               begin
                  if Attr.Is_Defined
                    and then not Attr.Values.Is_Empty
                  then
                     if not Bind (Idx).Is_Defined then
                        Bind (Idx).Initialize
                          (Source.Path_Name.Base_Filename,
                           View,
                           Has_Main       => False,
                           SAL_In_Closure => Has_SAL);

                        if not Tree_Db.Add_Action (Bind (Idx)) then
                           return False;
                        end if;

                        Tree_Db.Add_Input
                          (Link (Idx).UID,
                           Bind (Idx).Post_Bind.Object_File,
                           True);
                     end if;

                     for Val of Attr.Values loop
                        if Ada.Strings.Fixed.Index (Val.Text, "*") > 0 then
                           for CU of View.Units loop
                              if GNATCOLL.Utils.Match
                                (String (CU.Name), Val.Text)
                              then
                                 Add_CU (CU);
                              end if;
                           end loop;
                        else
                           CU := View.Unit (Name_Type (Val.Text));
                           Add_CU (CU);
                        end if;
                     end loop;
                  end if;
               end;
            end if;

            --  Add libraries

            for Lib of Libs.Serialize loop
               Tree_Db.Add_Input (Link (Idx).UID, Lib.Link.Output, True);
            end loop;

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
                  if not Archive.Is_Defined then
                     --  Need to create an intermediate library so that
                     --  foreign objects can be ignored by the linker
                     --  if no symbol is used from them. Else the linker
                     --  uses all objects that are on the command line.

                     Archive.Initialize_Global_Archive (View);

                     if not Tree_Db.Add_Action (Archive) then
                        return False;
                     end if;

                     for J in Link'Range loop
                        --  Add the archive only when the main is in Ada

                        Tree_Db.Add_Input (Link (J).UID, Archive.Output, True);
                     end loop;
                  end if;

                  Comp.Initialize (Src);

                  if not Tree_Db.Add_Action (Comp) then
                     return False;
                  end if;

                  if Comp.Object_File.Is_Defined then
                     Tree_Db.Add_Input (Archive.UID, Comp.Object_File, True);
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
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Options : Build.Options.Build_Options;
      Closure : in out GPR2.Project.View.Set.Object;
      Libs    : in out Library_Map.Map;
      Has_SAL : in out Boolean) return Boolean is
   begin
      return Populate_Withed_Projects_Internal
        (Tree_Db, Options, Closure, No_Library, Libs, Has_SAL);
   end Populate_Withed_Projects;

   ------------------------------
   -- Populate_Withed_Projects --
   ------------------------------

   function Populate_Withed_Projects
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Options : Build.Options.Build_Options;
      Closure : in out GPR2.Project.View.Set.Object;
      Ctxt    : Library_Type;
      Libs    : in out Library_Map.Map;
      Has_SAL : in out Boolean) return Boolean is
   begin
      return Populate_Withed_Projects_Internal
        (Tree_Db, Options, Closure, Ctxt, Libs, Has_SAL);
   end Populate_Withed_Projects;

   ---------------------------------------
   -- Populate_Withed_Projects_Internal --
   ---------------------------------------

   function Populate_Withed_Projects_Internal
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Options : Build.Options.Build_Options;
      Closure : in out GPR2.Project.View.Set.Object;
      Ctxt    : Library_Type;
      Libs    : in out Library_Map.Map;
      Has_SAL : in out Boolean) return Boolean
   is
      procedure Add_Deps (V  : GPR2.Project.View.Object);

      Todo         : GPR2.Project.View.Vector.Object;
      Seen         : GPR2.Project.View.Set.Object;

      --------------
      -- Add_Deps --
      --------------

      procedure Add_Deps (V : GPR2.Project.View.Object) is
      begin
         for Imp of V.Imports.Union (V.Limited_Imports) loop
            if not Seen.Contains (Imp) then
               Todo.Append (Imp);
            end if;
         end loop;

         if V.Is_Extending then
            for Ext of V.Extended loop
               for Imp of Ext.Imports.Union (Ext.Limited_Imports) loop
                  if not Seen.Contains (Imp) then
                     Todo.Append (Imp);
                  end if;
               end loop;
            end loop;
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

         case Current.Kind is
            when K_Standard =>
               if not Current.Is_Runtime then
                  Closure.Include (Current);
                  Add_Deps (Current);
               end if;

            when K_Abstract =>
               Add_Deps (Current);

            when K_Library | K_Aggregate_Library =>
               if not Populate_Library
                 (Tree_Db, Current, Options, Libs, Has_SAL)
               then
                  return False;
               end if;

               if Ctxt /= No_Library then
                  Libs.Add_Successor (Ctxt.View, Current);
               end if;

            when others =>
               null;
         end case;
      end loop;

      return True;
   end Populate_Withed_Projects_Internal;

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
