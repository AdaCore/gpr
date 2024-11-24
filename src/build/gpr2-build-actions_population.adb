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
      Options : Build_Options) return Boolean;

   function Populate_Withed_Units
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Visited : in out View_Ids.Set.Set) return Boolean;

   function As_Unit_Location
     (Basename  : Value_Type;
      Index     : Unit_Index;
      View      : GPR2.Project.View.Object;
      Recursive : Boolean) return Compilation_Unit.Unit_Location;

   ----------------------
   -- As_Unit_Location --
   ----------------------

   function As_Unit_Location
     (Basename : Value_Type;
      Index    : Unit_Index;
      View     : GPR2.Project.View.Object;
      Recursive : Boolean) return Compilation_Unit.Unit_Location
   is
      use Compilation_Unit;
      Src     : GPR2.Build.Source.Object;
      Tree_Db : constant GPR2.Build.Tree_Db.Object_Access :=
                  View.Tree.Artifacts_Database;
   begin
      if Recursive then
         Src := View.Visible_Source (Simple_Name (Basename));
      else
         Src := View.Source (Simple_Name (Basename));
      end if;

      if not Src.Is_Defined then
         for Lang of View.Language_Ids loop
            Src := View.Visible_Source
              (View.Suffixed_Simple_Name (Basename, Lang));
            exit when Src.Is_Defined;
         end loop;
      end if;

      if not Src.Is_Defined then
         Tree_Db.Reporter.Report
           (Message.Create
              (Message.Error,
               '"' & Basename &
                 """ was not found in the sources of any project",
               Source_Reference.Create (View.Path_Name.Value, 0, 0)));
         return No_Unit;
      end if;

      if Index /= No_Index then
         if not Src.Has_Units then
            Tree_Db.Reporter.Report
              (Message.Create
                 (Message.Error,
                  "unit index specified with a non unit-based source",
                  Source_Reference.Create (Src.Path_Name.Value, 0, 0)));
            return No_Unit;

         elsif not Src.Has_Unit_At (Index) then
            Tree_Db.Reporter.Report
              (Message.Create
                 (Message.Error,
                  " no unit for the index" & Index'Image,
                  Source_Reference.Create (Src.Path_Name.Value, 0, 0)));
            return No_Unit;
         end if;

      elsif Src.Has_Units
        and then not Src.Has_Single_Unit
      then
         Tree_Db.Reporter.Report
           (Message.Create
              (Message.Error,
               "multi-unit source used without a unit index",
               Source_Reference.Create (Src.Path_Name.Value, 0, 0)));
         return No_Unit;

      end if;

      return (View   => Src.Owning_View,
              Source => Src.Path_Name,
              Index  => Index);
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

      use type Ada.Containers.Count_Type;

   begin
      for V of Tree.Namespace_Root_Projects loop
         Visited.Insert (V.Id, Pos, Inserted);

         if not Options.Unique_Compilation
           and then not Options.Unique_Compilation_Recursive
         then
            --  Handle dependencies for the general case (no -u or -U options)
            Result := Populate_Withed_Units (Tree_Db, V, Visited);

            if not Result then
               return False;
            end if;
         end if;

         if Inserted then
            if Options.Unique_Compilation
              or else Options.Unique_Compilation_Recursive
            then
               --  Handle -u and -U:

               if Options.Mains.Is_Empty then
                  --  compile all sources, recursively in case -U is set
                  if Options.Unique_Compilation then
                     Result := Populate_All (Tree_Db, V, Actions_Set);
                  else
                     for C of V.Closure (True) loop
                        Result := Populate_All (Tree_Db, V, Actions_Set);
                        exit when not Result;
                     end loop;
                  end if;

                  return Result;

               else
                     --  Only compile the given sources
                  if Options.Mains.Length = 1 then
                     Mains.Append
                       (As_Unit_Location
                          (Options.Mains.First_Element,
                           Options.Unit_Index,
                           V,
                           Options.Unique_Compilation_Recursive));

                  else
                     for M of Options.Mains loop
                        Mains.Append
                          (As_Unit_Location
                             (M,
                              No_Index,
                              V,
                              Options.Unique_Compilation_Recursive));
                     end loop;
                  end if;

                  for M of Mains loop
                     if Options.Unique_Compilation_Recursive then
                        Src := V.Visible_Source (M.Source.Simple_Name);
                     else
                        Src := V.Source (M.Source.Simple_Name);
                     end if;

                     if Src.Language = Ada_Language then
                        declare
                           Comp : GPR2.Build.Actions.Compile.Ada.Object;
                        begin
                           Comp.Initialize (V.Unit (Src.Unit (M.Index).Name));
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
                  end loop;
               end if;

            else
               --  Handle general case:

               case V.Kind is
                  when K_Standard =>
                     if V.Has_Mains or else not Options.Mains.Is_Empty then
                        Result := Populate_Mains (Tree_Db, V, Options);
                     else
                        Result := Populate_All (Tree_Db, V, Actions_Set);
                     end if;

                  when K_Library =>
                     Result := Populate_Library (Tree_Db, V, Lib);

                  when K_Aggregate_Library =>
                     Result :=
                       Populate_Aggregated_Library (Tree_Db, V, Lib, Visited);

                  when K_Abstract =>
                     if not Options.Mains.Is_Empty then
                        for Main of Options.Mains loop
                           Tree.Reporter.Report
                             (GPR2.Message.Create
                                (GPR2.Message.Error,
                                 "cannot build '" & Main &
                                   "' with an abstract project",
                                 GPR2.Source_Reference.Create
                                   (V.Path_Name.Value, 0, 0)));
                        end loop;
                     end if;

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
                     Tree_Db.Remove_Action (A.UID);
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
         return Tree_Db.Propagate_Actions;
      else
         return True;
      end if;
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
      L : GPR2.Build.Actions.Link.Object;
   begin
      L.Initialize_Library (View);

      if not Tree_Db.Add_Action (L) then
         return False;
      end if;

      if not Tree_Db.Add_Output (L.UID, L.Output) then
         return False;
      end if;

      Lib := Artifacts.Library.Object (L.Output);

      --  ??? TODO: Take care of Library_Interface

      if not View.Is_Externally_Built then
         for CU of View.Own_Units loop
            declare
               Comp : GPR2.Build.Actions.Compile.Ada.Object;
            begin
               Comp.Initialize (CU);

               if not Tree_Db.Add_Action (Comp) then
                  return False;
               end if;

               Tree_Db.Add_Input
                 (L.UID, Comp.Object_File, False);
            end;
         end loop;

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

                  Tree_Db.Add_Input
                    (L.UID, Comp.Object_File, False);
               end;
            end if;
         end loop;
      end if;

      return True;
   end Populate_Library;

   --------------------
   -- Populate_Mains --
   --------------------

   function Populate_Mains
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Options : Build_Options) return Boolean
   is
      A_Comp     : Actions.Compile.Ada.Object;
      Comp       : Actions.Compile.Object;
      Source     : GPR2.Build.Source.Object;
      Mains      : GPR2.Build.Compilation_Unit.Unit_Location_Vector;
      Archive    : Actions.Link.Object;
      Closure    : GPR2.Project.View.Set.Object;
      Todo       : GPR2.Project.View.Set.Object;
      Seen       : GPR2.Project.View.Set.Object;

      use type Ada.Containers.Count_Type;
      use type Compilation_Unit.Unit_Location;
      use type GPR2.Path_Name.Object;

   begin
      --  Compute the actual list of Mains, that depend on command line or
      --  -if none provided- the attribute Main.

      if Options.Mains.Is_Empty then
         Mains := View.Mains;

      else
         if Options.Mains.Length = 1 then
            Mains.Append
              (As_Unit_Location
                 (Options.Mains.First_Element,
                  Options.Unit_Index,
                  View,
                  False));

         else
            for M of Options.Mains loop
               Mains.Append
                 (As_Unit_Location
                    (M,
                     No_Index,
                     View,
                     False));
            end loop;
         end if;

         --  Check that we could find all mains
         for M of Mains loop
            if M = Compilation_Unit.No_Unit then
               return False;
            end if;
         end loop;
      end if;

      --  Now process the mains one by one
      declare
         Bind   : array (1 .. Natural (Mains.Length)) of
                    Actions.Ada_Bind.Object;
         Link   : array (1 .. Natural (Mains.Length)) of Actions.Link.Object;
         Idx    : Natural := 1;
         Skip   : Boolean := False;
      begin
         for Main of Mains loop
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


         --  We calculate the closure so that we don't go through the
         --  library projects that already are self-contained

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

                  else
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

               for Main of Mains loop
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
