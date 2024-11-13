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
pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;
with GPR2.Project.View;
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
      View    : GPR2.Project.View.Object) return Boolean;

   function Populate_Withed_Units
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      View    : GPR2.Project.View.Object;
      Visited : in out View_Ids.Set.Set) return Boolean;

   ----------------------
   -- Populate_Actions --
   ----------------------

   function Populate_Actions
     (Tree : GPR2.Project.Tree.Object) return Boolean
   is
      Tree_Db     : GPR2.Build.Tree_Db.Object_Access renames
                      Tree.Artifacts_Database;
      Result      : Boolean := True;
      Visited     : View_Ids.Set.Set;
      Pos         : View_Ids.Set.Cursor;
      Inserted    : Boolean;
      Actions_Set : Actions.Sets.Set;
      Lib         : Artifacts.Library.Object;

   begin

      for V of Tree.Namespace_Root_Projects loop
         Visited.Insert (V.Id, Pos, Inserted);

         Result := Populate_Withed_Units (Tree_Db, V, Visited);

         if not Result then
            return False;
         end if;

         if Inserted then
            case V.Kind is
               when K_Standard =>
                  if V.Has_Mains then
                     Result := Populate_Mains (Tree_Db, V);
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
         end if;

         if Result then
            Result := Populate_Withed_Units (Tree_Db, V, Visited);
         end if;

         if not Result then
            return False;
         end if;
      end loop;

      return Tree_Db.Propagate_Actions;
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

               if not Tree_Db.Has_Action (Comp.UID)
                 and then not Tree_Db.Add_Action (Comp)
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

                  if not Tree_Db.Has_Action (Comp.UID) then
                     if not Tree_Db.Add_Action (Comp) then
                        return False;
                     end if;
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
      View    : GPR2.Project.View.Object) return Boolean
   is
      A_Comp  : Actions.Compile.Ada.Object;
      Comp    : Actions.Compile.Object;
      Bind    : Actions.Ada_Bind.Object;
      Link    : Actions.Link.Object;
      Source  : GPR2.Build.Source.Object;
      Is_Main : Boolean;

      use type GPR2.Path_Name.Object;

   begin
      for Main of View.Mains loop
         Source := Main.View.Source (Main.Source.Simple_Name);

         Link.Initialize_Executable
           (GPR2.Build.Artifacts.File_Part.Create (Main.Source, Main.Index),
            Main.View);

         if Tree_Db.Has_Action (Link.UID) then
            return True;

         elsif not Tree_Db.Add_Action (Link) then
            return False;
         end if;

         if Source.Language = Ada_Language then
            A_Comp.Initialize
              (View.Unit (Source.Units.Element (Main.Index).Name));

            if not Tree_Db.Has_Action (A_Comp.UID)
              and then not Tree_Db.Add_Action (A_Comp)
            then
               return False;
            end if;

            Bind.Initialize (A_Comp.Ali_File, Main.View);

            if not Tree_Db.Add_Action (Bind) then
               return False;
            end if;

            Tree_Db.Add_Input (Bind.UID, A_Comp.Ali_File, True);
            Tree_Db.Add_Input (Link.UID, A_Comp.Object_File, True);
            Tree_Db.Add_Input (Link.UID, Bind.Post_Bind.Object_File, True);

         else
            Comp.Initialize (Source);

            if not Tree_Db.Has_Action (Comp.UID)
              and then not Tree_Db.Add_Action (Comp)
            then
               return False;
            end if;

            Tree_Db.Add_Input (Link.UID, Comp.Object_File, True);

            return False;
         end if;

         --  Add all non-ada sources as dependency of the link as we cannot
         --  know the actual dependency

         for Src of View.Sources loop
            if not Src.Has_Units
              and then Src.Is_Compilable
              and then Src.Kind = S_Body
            then
               Is_Main := False;

               for M of View.Mains loop
                  if M.Source = Src.Path_Name then
                     Is_Main := True;
                     exit;
                  end if;
               end loop;

               if not Is_Main then
                  Comp.Initialize (Src);

                  if not Tree_Db.Has_Action (Comp.UID)
                    and then not Tree_Db.Add_Action (Comp)
                  then
                     return False;
                  end if;

                  Tree_Db.Add_Input (Link.UID, Comp.Object_File, True);
               end if;
            end if;
         end loop;

         --  Add the libraries present in the closure as dependencies

         for V of View.Closure loop
            if V.Is_Library then
               declare
                  Lib_Id : constant Actions.Link.Link_Id :=
                             Actions.Link.Create
                               (V, V.Library_Filename.Simple_Name, True);
                  Lib_A   : constant Actions.Link.Object'Class :=
                              Actions.Link.Object'Class
                                (Tree_Db.Action (Lib_Id));
               begin
                  Tree_Db.Add_Input (Link.UID, Lib_A.Output, True);

                  if Source.Language = Ada_Language then
                     Tree_Db.Add_Input (Bind.UID, Lib_A.Output, False);
                  end if;
               end;
            end if;
         end loop;
      end loop;

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
