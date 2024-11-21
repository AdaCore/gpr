--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Actions.Link;
with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Actions.Sets;
with GPR2.Build.Artifacts.File_Part;
with GPR2.Build.Artifacts.Library;
with GPR2.Build.Compilation_Unit;
with GPR2.Build.Compilation_Unit.Maps;
pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Build.Tree_Db;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.View.Set;
with GPR2.Source_Reference;
with GPR2.View_Ids.Set;

package body GPR2.Build.Actions_Population is

   function As_Unit_Location
     (Basename       : Value_Type;
      Index          : Unit_Index;
      View           : GPR2.Project.View.Object;
      Options        : Build_Options;
      Error_Reported : out Boolean)
      return Compilation_Unit.Unit_Location_Vector;

   function Populate_Aggregated_Library
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Lib     : out Artifacts.Library.Object;
      Visited : in out GPR2.View_Ids.Set.Set) return Boolean;

   function Populate_All
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Actions : out Build.Actions.Sets.Set) return Boolean;

   function Populate_Library
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Lib     : out Artifacts.Library.Object) return Boolean;

   function Populate_Mains
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Mains   : GPR2.Build.Compilation_Unit.Unit_Location_Vector;
      Options : Build_Options) return Boolean;

   function Populate_Withed_Units
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Visited : in out View_Ids.Set.Set) return Boolean;

   ----------------------
   -- As_Unit_Location --
   ----------------------

   function As_Unit_Location
     (Basename       : Value_Type;
      Index          : Unit_Index;
      View           : GPR2.Project.View.Object;
      Options        : Build_Options;
      Error_Reported : out Boolean)
      return Compilation_Unit.Unit_Location_Vector
   is
      use Compilation_Unit;
      Src     : GPR2.Build.Source.Object;
      Tree_Db : constant GPR2.Build.Tree_Db.Object_Access :=
                  View.Tree.Artifacts_Database;
      Res     : Unit_Location_Vector;
   begin
      Error_Reported := False;

      if Options.Unique_Compilation
        or else Options.Unique_Compilation_Recursive
      then
         Src := View.Visible_Source
           (Path_Name.Simple_Name (Filename_Type (Basename)));
      else
         Src := View.Source
           (Path_Name.Simple_Name (Filename_Type (Basename)));
      end if;

      if not Src.Is_Defined then
         for Lang of View.Language_Ids loop
            Src := View.Visible_Source
              (View.Suffixed_Simple_Name (Basename, Lang));
            exit when Src.Is_Defined;
         end loop;
      end if;

      if not Src.Is_Defined then
         return Compilation_Unit.Empty_Vector;
      end if;

      if Src.Owning_View.Is_Library
        and then not Options.Unique_Compilation
        and then not Options.Unique_Compilation_Recursive
      then
         Tree_Db.Reporter.Report
           (Message.Create
              (Message.Error,
               "main cannot be a source of a library project: """ &
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
      Options : Build_Options) return Boolean
   is
      Tree_Db     : GPR2.Build.Tree_Db.Object_Access renames
                      Tree.Artifacts_Database;
      Result      : Boolean := True;
      Visited     : View_Ids.Set.Set;
      Pos         : View_Ids.Set.Cursor;
      Inserted    : Boolean;
      Actions_Set : Actions.Sets.Set;
      Lib         : Artifacts.Library.Object;
      Src         : GPR2.Build.Source.Object;
      Mains       : GPR2.Build.Compilation_Unit.Unit_Location_Vector;
      To_Remove   : Actions.Sets.Set;
      Has_Error   : Boolean;

   begin
      --  Lookup the source(s) given explicitly on the command line, if any.

      Inserted := False;

      for Main of Options.Mains loop
         for V of Tree.Namespace_Root_Projects loop
            declare
               M : constant Compilation_Unit.Unit_Location_Vector :=
                     As_Unit_Location
                       (Main,
                        Options.Unit_Index,
                        V,
                        Options,
                        Has_Error);
            begin
               if Has_Error then
                  return False;
               end if;

               if not M.Is_Empty then
                  Inserted := True;
                  Mains.Append (M);
               end if;
            end;
         end loop;

         if not Inserted then
            if Options.Unique_Compilation
              or else Options.Unique_Compilation_Recursive
            then
               Tree_Db.Reporter.Report
                 (Message.Create
                    (Message.Error,
                     '"' & Main &
                       """ was not found in the sources of any project",
                     Source_Reference.Create
                       (Tree.Root_Project.Path_Name.Value, 0, 0)));
            else
               Tree_Db.Reporter.Report
                 (Message.Create
                    (Message.Error,
                     '"' & Main &
                       """ was not found in the project",
                     Source_Reference.Create
                       (Tree.Root_Project.Path_Name.Value, 0, 0)));
            end if;

            return False;
         end if;
      end loop;

      for V of Tree.Namespace_Root_Projects loop
         Visited.Insert (V.Id, Pos, Inserted);

         if Inserted then
            if Options.Unique_Compilation
              or else Options.Unique_Compilation_Recursive
            then
               --  Handle -u and -U:

               if Mains.Is_Empty then
                  --  compile all sources, recursively in case -U is set
                  if Options.Unique_Compilation then
                     Result := Populate_All (Tree_Db, V, Actions_Set);
                  else
                     for C of V.Closure (True) loop
                        if not C.Is_Externally_Built then
                           Result := Populate_All (Tree_Db, C, Actions_Set);
                           exit when not Result;
                        end if;
                     end loop;
                  end if;

                  return Result;

               else
                  --  Only compile the given sources

                  for M of Mains loop
                     Src := V.Visible_Source (M.Source.Simple_Name);

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
               --  Handle general case:

               --  Make sure the withed libraries are added to the tree
               Result := Populate_Withed_Units (Tree_Db, V, Visited);

               case V.Kind is
                  when K_Standard =>
                     if V.Has_Mains or else not Mains.Is_Empty then
                        Result := Populate_Mains (Tree_Db, V, Mains, Options);
                     else
                        Result := Populate_All (Tree_Db, V, Actions_Set);
                     end if;

                  when K_Library =>
                     Result := Populate_Library (Tree_Db, V, Lib);

                  when K_Aggregate_Library =>
                     Result :=
                       Populate_Aggregated_Library (Tree_Db, V, Lib, Visited);

                  when others =>
                     null;
               end case;

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

                  for A of To_Remove loop
                     Tree_Db.Action_Id_To_Reference (A.UID).Deactivate;
                  end loop;
               end if;
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

      if Result then
         Tree_Db.Load_Signatures;
      end if;

      return Result;
   end Populate_Actions;

   ---------------------------------
   -- Populate_Aggregated_Library --
   ---------------------------------

   function Populate_Aggregated_Library
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Lib     :    out Artifacts.Library.Object;
      Visited : in out GPR2.View_Ids.Set.Set) return Boolean
   is
      L          : GPR2.Build.Actions.Link.Object;
      Action_Set : Actions.Sets.Set;
      Result     : Boolean;
      Agg_Lib    : Artifacts.Library.Object;

   begin
      L.Initialize_Library (View);

      if not Tree_Db.Add_Action (L) then
         return False;
      end if;

      if not Tree_Db.Add_Output (L.UID, L.Output) then
         return False;
      end if;

      Lib := Artifacts.Library.Object (L.Output);

      for Agg of View.Aggregated loop
         Visited.Include (Agg.Id);
         Result := True;

         case Agg.Kind is
            when K_Standard =>
               Result := Populate_All (Tree_Db, Agg, Action_Set);

               for A of Action_Set loop
                  Tree_Db.Add_Input
                    (L.UID,
                     Actions.Compile.Object'Class (A).Object_File,
                     True);
               end loop;

            when K_Library =>
               Result := Populate_Library (Tree_Db, Agg, Agg_Lib);
               Tree_Db.Add_Input (L.UID, Agg_Lib, True);

            when K_Aggregate_Library =>
               Result :=
                 Populate_Aggregated_Library (Tree_Db, Agg, Agg_Lib, Visited);
               Tree_Db.Add_Input (L.UID, Agg_Lib, True);

            when others =>
               null;
         end case;

         if not Result then
            return False;
         end if;
      end loop;

      return True;
   end Populate_Aggregated_Library;

   ------------------
   -- Populate_All --
   ------------------

   function Populate_All
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Actions : out Build.Actions.Sets.Set) return Boolean
   is
   begin
      if View.Is_Externally_Built then
         --  Nothing to do
         return True;
      end if;

      declare
         Comp : GPR2.Build.Actions.Compile.Ada.Object;
      begin
         for CU of View.Own_Units loop
            Comp.Initialize (CU);

            if not Tree_Db.Add_Action (Comp) then
               return False;
            end if;

            Actions.Include (Comp);
         end loop;
      end;

      declare
         Comp : GPR2.Build.Actions.Compile.Object;
      begin
         for Src of View.Sources loop
            if not Src.Has_Units
              and then Src.Is_Compilable
              and then Src.Kind = S_Body
            then
               Comp.Initialize (Src);

               if not Tree_Db.Add_Action (Comp)
               then
                  return False;
               end if;

               Actions.Include (Comp);
            end if;
         end loop;
      end;

      return True;
   end Populate_All;

   ----------------------
   -- Populate_Library --
   ----------------------

   function Populate_Library
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Lib     : out Artifacts.Library.Object) return Boolean
   is
      use GPR2.Build.Compilation_Unit;

      procedure Interface_Units
        (Units : out GPR2.Build.Compilation_Unit.Maps.Map);
      --  Provide the compilation units provided by either Library_Interface
      --  or Interfaces attributes.

      function Add_Comp_To_Tree_And_Linker
        (CU     : GPR2.Build.Compilation_Unit.Object;
         Linker : GPR2.Build.Actions.Link.Object) return Boolean;
      --  Create an Ada compile action from the provided CU, add its to the
      --  tree and add its object file as a linker input.
      --  Return True on success.

      ---------------------------------
      -- Add_Comp_To_Tree_And_Linker --
      ---------------------------------

      function Add_Comp_To_Tree_And_Linker
        (CU     : GPR2.Build.Compilation_Unit.Object;
         Linker : GPR2.Build.Actions.Link.Object) return Boolean
      is
         Comp : GPR2.Build.Actions.Compile.Ada.Object;
      begin
         Comp.Initialize (CU);

         if not Tree_Db.Add_Action (Comp) then
            return False;
         end if;

         Tree_Db.Add_Input (Linker.UID, Comp.Object_File, False);
         return True;
      end Add_Comp_To_Tree_And_Linker;

      ---------------------
      -- Interface_Units --
      ---------------------

      procedure Interface_Units
        (Units : out GPR2.Build.Compilation_Unit.Maps.Map)
      is
         use GPR2.Build.Compilation_Unit.Maps;
         CU : GPR2.Build.Compilation_Unit.Object;
         use GPR2.Containers;
      begin
         Units := Empty_Map;

         if View.Has_Library_Interface then
            for Unit_Name_Curs in View.Interface_Units.Iterate loop
               CU :=
                 View.Own_Unit
                   (Name => Unit_Name_To_Sloc.Key (Unit_Name_Curs));
               Units.Include (CU.Name, CU);
            end loop;
         else
            pragma Assert (View.Has_Interfaces);
            for Interface_Source_Cursor in View.Interface_Sources.Iterate loop
               declare
                  Source_Name : constant Simple_Name :=
                    Simple_Name
                      (Source_Path_To_Sloc.Key (Interface_Source_Cursor));
                  Source      : constant GPR2.Build.Source.Object :=
                    View.Source (Filename => Source_Name);

               begin
                  --  If the file does not contain units, it does not need
                  --  elaboration nor finalization.

                  if not Source.Has_Units then
                     goto Next_Interface;
                  end if;

                  if Source.Has_Single_Unit then
                     CU := View.Own_Unit (Name => Source.Unit.Name);
                     Units.Include (CU.Name, CU);
                  else
                     --  Process all the units contained in the source file.
                     --  An ALI file is generated for each unit. If a source is
                     --  multi units, then a ~<index> suffix is added to the
                     --  source name to produce the ali file name.For instance,
                     --  if the file foo.adb contains unit A and B, then two
                     --  files will be produced: foo~1.ali and foo~2.ali.

                     for Unit_Info of Source.Units loop
                        CU := View.Own_Unit (Name => Unit_Info.Name);
                        Units.Include (CU.Name, CU);
                     end loop;
                  end if;
               end;

               <<Next_Interface>>
            end loop;
         end if;
      end Interface_Units;

      L            : GPR2.Build.Actions.Link.Object;
      Bind         : GPR2.Build.Actions.Ada_Bind.Object;
      Interf_Units : GPR2.Build.Compilation_Unit.Maps.Map;
   begin
      L.Initialize_Library (View);

      if not Tree_Db.Add_Action (L) then
         return False;
      end if;

      if not Tree_Db.Add_Output (L.UID, L.Output) then
         return False;
      end if;

      Lib := Artifacts.Library.Object (L.Output);

      if View.Is_Externally_Built then
         return True;
      end if;

      if View.Is_Library_Standalone then

         --  Create the binder action that will create the file in charge of
         --  elaborating and finalizing the lib. Used for standalone libraries.

         declare
            Binding_Extra_Opts : GPR2.Containers.Value_List :=
              GPR2.Containers.Empty_Value_List;
         begin
            Binding_Extra_Opts.Append ("-a");
            Binding_Extra_Opts.Append ("-n");

            Bind.Initialize
              (Basename   => View.Library_Name,
               Context    => View,
               Extra_Opts => Binding_Extra_Opts);
         end;

         if not Tree_Db.Add_Action (Bind) then
            return False;
         end if;

         Tree_Db.Add_Input (L.UID, Bind.Post_Bind.Object_File, True);
         Interface_Units (Interf_Units);

         for CU of Interf_Units loop
            declare
               Comp : GPR2.Build.Actions.Compile.Ada.Object;
            begin
               Comp.Initialize (CU);

               if not Tree_Db.Add_Action (Comp) then
                  return False;
               end if;

               Tree_Db.Add_Input (L.UID, Comp.Object_File, False);
               Tree_Db.Add_Input (Bind.UID, Comp.Ali_File, True);

               if Interf_Units.First_Element = CU then
                  Tree_Db.Add_Input (L.UID, Bind.Post_Bind.Object_File, True);
               end if;
            end;
         end loop;
      else
         for CU of View.Own_Units loop
            if not Add_Comp_To_Tree_And_Linker (CU, L) then
               return False;
            end if;
         end loop;
      end if;

      for Src of View.Sources loop
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

               Tree_Db.Add_Input (L.UID, Comp.Object_File, False);
            end;
         end if;
      end loop;

      return True;
   end Populate_Library;

   --------------------
   -- Populate_Mains --
   --------------------

   function Populate_Mains
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Mains   : GPR2.Build.Compilation_Unit.Unit_Location_Vector;
      Options : Build_Options) return Boolean
   is
      A_Comp     : Actions.Compile.Ada.Object;
      Comp       : Actions.Compile.Object;
      Source     : GPR2.Build.Source.Object;
      Archive    : Actions.Link.Object;
      Closure    : GPR2.Project.View.Set.Object;
      Todo       : GPR2.Project.View.Set.Object;
      Seen       : GPR2.Project.View.Set.Object;
      Actual_Mains : Compilation_Unit.Unit_Location_Vector;

      use type GPR2.Path_Name.Object;
      use type Ada.Containers.Count_Type;

   begin
      if Mains.Is_Empty then
         Actual_Mains := View.Mains;
      else
         Actual_Mains := Mains;
      end if;

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

      --  Process the mains one by one
      declare
         Bind   : array (1 .. Natural (Actual_Mains.Length)) of
                    Actions.Ada_Bind.Object;
         Link   : array (1 .. Natural (Actual_Mains.Length)) of
                    Actions.Link.Object;
         Idx    : Natural := 1;
         Skip   : Boolean := False;
      begin
         for Main of Actual_Mains loop
            Source := Main.View.Source (Main.Source.Simple_Name);

            Link (Idx).Initialize_Executable
              (GPR2.Build.Artifacts.File_Part.Create (Main.Source, Main.Index),
               Main.View,
               -Options.Output_File);

            if not Tree_Db.Has_Action (Link (Idx).UID)
              and then not Tree_Db.Add_Action (Link (Idx))
            then
               return False;
            end if;

            if Source.Language = Ada_Language then
               A_Comp.Initialize
                 (View.Unit (Source.Units.Element (Main.Index).Name));

               if not Tree_Db.Add_Action (A_Comp) then
                  return False;
               end if;

               Bind (Idx).Initialize
                 (A_Comp.Ali_File.Path.Base_Filename, Main.View);

               if not Tree_Db.Add_Action (Bind (Idx)) then
                  return False;
               end if;

               Tree_Db.Add_Input (Bind (Idx).UID, A_Comp.Ali_File, True);
               Tree_Db.Add_Input (Link (Idx).UID, A_Comp.Object_File, True);
               Tree_Db.Add_Input
                 (Link (Idx).UID, Bind (Idx).Post_Bind.Object_File, True);

            else
               Comp.Initialize (Source);

               if not Tree_Db.Add_Action (Comp) then
                  return False;
               end if;

               Tree_Db.Add_Input (Link (Idx).UID, Comp.Object_File, True);

               --  In case the main is non-ada and we have Ada sources in
               --  the view, we need to add a binding phase and an explicit
               --  dependency from the link phase to the ada objects.

               if View.Language_Ids.Contains (Ada_Language) then
                  --  Make sure we have a binding phase for the ada sources
                  --  that generates a binder file with external main.

                  Bind (Idx).Initialize
                    (Source.Path_Name.Base_Filename,
                     View,
                     "-n");

                  if not Tree_Db.Add_Action (Bind (Idx)) then
                     return False;
                  end if;

                  Tree_Db.Add_Input
                    (Link (Idx).UID, Bind (Idx).Post_Bind.Object_File, True);

                  for U of View.Own_Units loop
                     A_Comp.Initialize (U);

                     if not Tree_Db.Add_Action (A_Comp) then
                        return False;
                     end if;

                     Tree_Db.Add_Input (Bind (Idx).UID, A_Comp.Ali_File, True);
                     Tree_Db.Add_Input
                       (Link (Idx).UID, A_Comp.Object_File, True);
                  end loop;
               end if;
            end if;

            Idx := Idx + 1;
         end loop;

         --  Now add to each link/bind action dependencies to the libraries

         for V of View.Closure loop
            if V.Is_Library then
               --  Add the libraries present in the closure as
               --  dependencies.

               declare
                  Lib_Id  : constant Actions.Link.Link_Id :=
                              Actions.Link.Create
                                (V,
                                 V.Library_Filename.Simple_Name,
                                 True);
                  Lib_A   : constant Actions.Link.Object'Class :=
                              Actions.Link.Object'Class
                                (Tree_Db.Action (Lib_Id));
               begin
                  for J in Link'Range loop
                     Tree_Db.Add_Input
                       (Link (J).UID, Lib_A.Output, True);

                     if Bind (J).Is_Defined then
                        Tree_Db.Add_Input
                          (Bind (J).UID, Lib_A.Output, False);
                     end if;
                  end loop;
               end;
            end if;
         end loop;

         --  Now we need to add the objects from all regular views, filtering
         --  out libraries (and their imports)

         Closure.Clear;
         Seen.Clear;
         Todo.Insert (View);
         Todo.Union (View.Imports);
         Todo.Union (View.Limited_Imports);

         while not Todo.Is_Empty loop
            declare
               V : constant GPR2.Project.View.Object :=
                     Todo.First_Element;
            begin
               Todo.Delete_First;

               if not Seen.Contains (V) then
                  Seen.Insert (V);

                  if not V.Is_Library then
                     Closure.Include (V);

                     Todo.Union (V.Imports.Difference (Seen));
                     Todo.Union (V.Limited_Imports.Difference (Seen));
                  end if;
               end if;
            end;
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

                  Tree_Db.Add_Input (Archive.UID, Comp.Object_File, True);
               end if;
            end loop;
         end loop;
      end;

      return True;
   end Populate_Mains;

   ---------------------------
   -- Populate_Withed_Units --
   ---------------------------

   function Populate_Withed_Units
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Visited : in out View_Ids.Set.Set) return Boolean
   is
      Lib    : Artifacts.Library.Object;
      Result : Boolean;
   begin
      for Import of View.Imports.Union (View.Limited_Imports) loop
         Result := True;

         if not Visited.Contains (Import.Id) then
            Visited.Include (Import.Id);

            if Import.Kind = K_Library then
               Result :=
                 Populate_Library (Tree_Db, Import, Lib);
            elsif Import.Kind = K_Aggregate_Library then
               Result :=
                 Populate_Aggregated_Library (Tree_Db, Import, Lib, Visited);
            end if;

            if not Result then
               return False;
            end if;
         end if;

         Result := Populate_Withed_Units (Tree_Db, Import, Visited);

         if not Result then
            return False;
         end if;
      end loop;

      return True;
   end Populate_Withed_Units;

end GPR2.Build.Actions_Population;
