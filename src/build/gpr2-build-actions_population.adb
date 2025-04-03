--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Vectors;
with Ada.Strings.Fixed;

with GNATCOLL.Traces;

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

   type Library_Type is record
      View : GPR2.Project.View.Object;
      Link : GPR2.Build.Actions.Link.Object;
      Bind : GPR2.Build.Actions.Ada_Bind.Object;
   end record;

   package View_Id_Library_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => View_Ids.View_Id,
      Element_Type => Library_Type,
      "<"          => View_Ids."<");
   package Library_Vector is new Ada.Containers.Vectors
     (Positive, Library_Type);

   package Library_Map is
      type Map is tagged limited record
         Values  : View_Id_Library_Map.Map;
         Vector  : Library_Vector.Vector;
      end record;

      function Contains
        (Self : Map; Key : GPR2.Project.View.Object) return Boolean
      is (Self.Values.Contains (Key.Id));

      procedure Include
        (Self    : in out Map;
         Element : Library_Type);

      procedure Update
        (Self    : in out Map;
         Element : Library_Type);
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

   package body Library_Map is

      -------------
      -- Include --
      -------------

      procedure Include
        (Self    : in out Map;
         Element : Library_Type)
      is
      begin
         Self.Values.Include (Element.View.Id, Element);
         Self.Vector.Append (Element);
      end Include;

      ------------
      -- Update --
      ------------

      procedure Update
        (Self    : in out Map;
         Element : Library_Type)
      is
         Old : constant View_Id_Library_Map.Cursor :=
                 Self.Values.Find (Element.View.Id);
         Idx : constant Positive :=
                 Self.Vector.Find_Index (View_Id_Library_Map.Element (Old));
      begin
         Self.Values.Replace_Element (Old, Element);
         Self.Vector.Replace_Element (Idx, Element);
      end Update;

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
     (Tree           : GPR2.Project.Tree.Object;
      Options        : Build.Options.Build_Options;
      Static_Actions : Boolean) return Boolean
   is
      Tree_Db   : GPR2.Build.Tree_Db.Object_Access renames
                    Tree.Artifacts_Database;
      Result    : Boolean := True;
      Visited   : View_Ids.Set.Set;
      Pos       : View_Ids.Set.Cursor;
      Inserted  : Boolean;
      Libs      : Library_Map.Map;
      Src       : GPR2.Build.Source.Object;
      Mains     : GPR2.Build.Compilation_Unit.Unit_Location_Vector;
      To_Remove : Actions.Sets.Set;
      Has_Error : Boolean;
      Has_SAL   : Boolean := False;
      Closure   : GPR2.Project.View.Set.Object;
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
         declare
            List : Build.Tree_Db.Actions_List'Class :=
                     Tree_Db.All_Actions;
         begin
            for C in List.Action_Iterate loop
               declare
                  Action : constant Build.Tree_Db.Action_Reference_Type :=
                             List.Action_Reference (C);
               begin
                  if not Action.On_Ready_State then
                     return False;
                  end if;
               end;
            end loop;
         end;
      end if;

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

   begin
      if View.Is_Extended or else Libs.Contains (View) then
         --  Extended library projects won't produce any library, so skip
         --  them. Also skip already analyzed library projects.
         return True;
      end if;

      Self.View := View;
      Self.Link.Initialize_Library (View, Options.No_Run_Path);

      if not Tree_Db.Add_Action (Self.Link) then
         return False;
      end if;

      --  Add the lib now to prevent infinite recursion in case of
      --  circular dependencies (e.g. A withes B that limited_withes A)

      Libs.Include (Self);

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
        (Tree_Db, Options, Closure, Libs, Has_SAL)
      then
         return False;
      end if;

      SAL_Closure :=
        SAL_Closure or else Has_SAL or else View.Is_Library_Standalone;

      if View.Is_Externally_Built then
         return True;
      end if;

      if View.Is_Library_Standalone
        and then
          (View.Language_Ids.Contains (Ada_Language)
           or else
             (View.Kind = K_Aggregate_Library
              and then
                (for some Agg of View.Aggregated =>
                     Agg.Language_Ids.Contains (Ada_Language))))
      then
         --  Create the binder action that will create the file in charge of
         --  elaborating and finalizing the lib. Used for standalone libraries.

         Self.Bind.Initialize
           (Basename       => View.Library_Name,
            Context        => View,
            Has_Main       => False,
            SAL_In_Closure => Has_SAL,
            Skip           => Options.No_SAL_Binding);

         if not Tree_Db.Add_Action (Self.Bind) then
            return False;
         end if;

         Tree_Db.Add_Input
           (Self.Link.UID, Self.Bind.Post_Bind.Object_File, True);

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
               Tree_Db.Add_Input (Self.Bind.UID, Comp.Local_Ali_File, True);
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
      --  on the link to make sure the other library is build first, but also
      --  on the eventual corresponding binding action so that all ali files
      --  are present in the Library_Ali_Dir at the moment we bind

      declare
         C : View_Id_Library_Map.Cursor;
      begin
         for Import of Self.View.Imports loop
            if Import.Is_Library then
               C := Libs.Values.Find (Import.Id);

               if not View_Id_Library_Map.Has_Element (C) then
                  Traces.Trace ("Error: could not find " & String (Import.Name)
                                & "in the list of library dependencies");
                  return False;
               end if;

               declare
                  Lib : constant Library_Type :=
                          View_Id_Library_Map.Element (C);
               begin
                  Tree_Db.Add_Input
                    (Self.Link.UID,
                     Lib.Link.Output, True);

                  if Self.Bind.Is_Defined then
                     --  Make sure the libraries dependencies are bounded
                     --  before the binder is called, as it's the bind action
                     --  that copies the ali files to the library directory.

                     Tree_Db.Add_Input
                       (Self.Bind.UID, Lib.Link.Output, False);
                  end if;
               end;
            end if;
         end loop;
      end;

      --  Update the Library object in Libs

      Libs.Update (Self);

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
               Options.No_Run_Path,
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

                  Attr : constant GPR2.Project.Attribute.Object :=
                           View.Attribute
                             (PRA.Roots,
                              PAI.Create_Source (Main.Source.Simple_Name));
                  CU   : GPR2.Build.Compilation_Unit.Object;

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

            for Lib of Libs.Vector loop
               Tree_Db.Add_Input (Link (Idx).UID, Lib.Link.Output, True);

               --  Make sure the bind action is executed after the libraries
               --  are linked, to have access to the ALI files in the lib
               --  directory.
               if Bind (Idx).Is_Defined then
                  Tree_Db.Add_Input
                    (Bind (Idx).UID, Lib.Link.Output, False);
               end if;
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
      Has_SAL : in out Boolean) return Boolean
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

         if Current.Is_Library then
            if not Populate_Library
              (Tree_Db, Current, Options, Libs, Has_SAL)
            then
               return False;
            end if;

         elsif Current.Kind = K_Abstract then
               Add_Deps (Current);

         elsif Current.Kind /= K_Configuration
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
