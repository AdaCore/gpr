--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);

with GPR2.Build.View_Tables;
with GPR2.Containers;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.View_Ids.Set;

package body GPR2.Build.Tree_Db is

   package PRA renames GPR2.Project.Registry.Attribute;

   use type GPR2.View_Ids.View_Id;

   procedure Add_Dependency
     (Self        : access Object;
      Artifact    : Artifact_Ids.Artifact_Id;
      Predecessor : Artifact_Ids.Artifact_Id;
      Kind        : Dependency_Kind);

   procedure Release (Self : access Object);
   --  To be called after a refresh to update the DAG

   ------------------
   -- Add_Artifact --
   ------------------

   procedure Add_Artifact
     (Self     : access Object;
      Artifact : Build.Artifacts.Object'Class)
   is
      C    : Artifact_Class_Artifact_Set_Maps.Cursor;
      Done : Boolean;
      Id   : constant Artifact_Id := Artifact.Id;
   begin
      Self.Artifacts.Insert (Id, Artifact);

      --  If key already exists, C is positioned to it, else it is created
      --  with an empty artifacts set.
      Self.New_Artifacts.Insert
        (Artifact.Class, Artifact_Sets.Empty_Set, C, Done);

      Self.New_Artifacts.Reference (C).Include (Id);
   end Add_Artifact;

   --------------------
   -- Add_Dependency --
   --------------------

   procedure Add_Dependency
     (Self        : access Object;
      Artifact    : Artifact_Ids.Artifact_Id;
      Predecessor : Artifact_Ids.Artifact_Id;
      Kind        : Dependency_Kind)
   is
      C    : Artifact_Dependency_Maps.Cursor;
      Done : Boolean;

   begin
      --  Either add a new clean dependencies record, or set C to its existing
      --  position
      Self.Predecessors.Insert
        (Artifact, Dependencies'(others => <>), C, Done);
      Self.Predecessors.Reference (C).Inputs (Kind).Include (Predecessor);

      --  Do the same for the Successors list
      Self.Successors.Insert
        (Predecessor, Dependencies'(others => <>), C, Done);
      Self.Successors.Reference (C).Inputs (Kind).Include (Artifact);

      Self.Sort_Valid := False;
   end Add_Dependency;

   -----------------------------
   -- Add_Explicit_Dependency --
   -----------------------------

   procedure Add_Explicit_Dependency
     (Self        : access Object;
      Artifact    : Artifact_Ids.Artifact_Id;
      Predecessor : Artifact_Ids.Artifact_Id) is
   begin
      Self.Add_Dependency (Artifact, Predecessor, Explicit);
   end Add_Explicit_Dependency;

   -----------------------------
   -- Add_Implicit_Dependency --
   -----------------------------

   procedure Add_Implicit_Dependency
     (Self        : access Object;
      Artifact    : Artifact_Ids.Artifact_Id;
      Predecessor : Artifact_Ids.Artifact_Id)
   is
   begin
      Self.Add_Dependency (Artifact, Predecessor, Implicit);
   end Add_Implicit_Dependency;

   ---------------
   -- Artifacts --
   ---------------

   function Artifacts
     (Self  : Object;
      Class : Artifact_Class := No_Artifact_Class)
      return Iterator'Class
   is
   begin
      return Iterator'
        (Kind   => Global_List,
         Actual => (Kind  => Global_List,
                    Graph => Self'Unrestricted_Access,
                    Class => Class));
   end Artifacts;

   ----------------
   -- Check_Tree --
   ----------------

   procedure Check_Tree (Self : in out Object) is
      To_Remove : GPR2.View_Ids.Set.Set;
   begin
      --  Check for new views

      for V of Self.Tree.Ordered_Views loop
         if V.Kind in With_Object_Dir_Kind
           and then not Self.Build_Dbs.Contains (V.Id)
         then
            declare
               Db_Data : View_Tables.View_Data
                           (Is_Root => V.Is_Namespace_Root);
               Db_Inst : View_Db.Object;
            begin
               Db_Data.View    := V;
               Db_Data.Tree_Db := Self.Self;
               Db_Inst := View_Tables.View_Base_For (Db_Data);
               Self.Build_Dbs.Insert (V.Id, Db_Inst);
               --  Db_Inst.Update;
            end;
         end if;
      end loop;

      --  Check for deleted views

      for C in Self.Build_Dbs.Iterate loop
         declare
            Id : constant View_Ids.View_Id := Build_DB_Maps.Key (C);
         begin
            if not Self.Tree.Get_View (Id).Is_Defined then
               To_Remove.Include (Id);
            end if;
         end;
      end loop;

      for Id of To_Remove loop
         Self.Build_Dbs.Delete (Id);
      end loop;
   end Check_Tree;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self : aliased Object; Position : Cursor) return Constant_Reference_Type
   is
      Ref : constant Artifact_Maps.Constant_Reference_Type :=
              Self.Artifacts.Constant_Reference (Position.C);
   begin
      return (Element => Ref.Element.all'Unchecked_Access,
              Ref     => Ref);
   end Constant_Reference;

   ----------
   -- Load --
   ----------

   procedure Create
     (Self                 : in out Object;
      Tree                 : GPR2.Project.Tree.Object;
      With_Runtime_Sources : Boolean)
   is
      Db_Inst : View_Db.Object;

   begin
      Self.Self := Self'Unrestricted_Access;
      Self.Tree := Tree.Reference;
      Self.With_RTS := With_Runtime_Sources;

      --  Source files are propagated from the source owner (e.g. the view that
      --  defines the source directory where we found the source) to
      --  the other views (aggregate libraries or extending projects).
      --
      --  So for this to work efficiently, we need to use a topological order
      --  to populate the sources.

      for V of Tree.Ordered_Views loop
         if V.Kind in With_Object_Dir_Kind then
            declare
               Db_Data : View_Tables.View_Data
                           (Is_Root => V.Is_Namespace_Root);
            begin
               Db_Data.View    := V;
               Db_Data.Tree_Db := Self.Self;
               Db_Inst := View_Tables.View_Base_For (Db_Data);
               Self.Build_Dbs.Insert (V.Id, Db_Inst);
            end;
         end if;
      end loop;
   end Create;

   -----------
   -- First --
   -----------

   overriding function First (Self : Artifact_Iterator) return Cursor is
      C : Cursor (Self.Kind);
   begin
      case Self.Kind is
         when Global_List =>
            if Self.Class = No_Artifact_Class then
               C.C := Self.Graph.Artifacts.First;

            else
               C.C := Artifact_Maps.No_Element;

               for Pos in Self.Graph.Artifacts.Iterate loop
                  if Artifact_Ids.Class (Artifact_Maps.Key (Pos)) =
                       Self.Class
                  then
                     C.C := Pos;

                     exit;
                  end if;
               end loop;
            end if;

         when Predecessors | Successors =>
            if not Artifact_Dependency_Maps.Has_Element (Self.Pos) then
               return No_Element;
            end if;

            declare
               subtype CR_Type is
                 Artifact_Dependency_Maps.Constant_Reference_Type;
               Ref   : constant CR_Type :=
                         (if Self.Kind = Predecessors
                          then Self.Graph.Predecessors.Constant_Reference
                            (Self.Pos)
                          else Self.Graph.Successors.Constant_Reference
                            (Self.Pos));
               Found : Boolean := False;
            begin
               for Kind in Dependency_Kind'Range loop
                  if not Ref.Inputs (Kind).Is_Empty then
                     C.C_Art (Kind) := Ref.Inputs (Kind).First;
                     if not Found then
                        C.Current := Kind;
                        C.C := Self.Graph.Artifacts.Find
                          (Ref.Inputs (Kind).Element (C.C_Art (Kind)));
                        Found := True;
                     end if;
                  end if;
               end loop;

               if Found then
                  return C;
               else
                  return No_Element;
               end if;
            end;
      end case;

      return C;
   end First;

   -----------------------------
   -- Iter_Constant_Reference --
   -----------------------------

   function Iter_Constant_Reference
     (Self     : aliased Iterator;
      Position : Cursor) return Constant_Reference_Type
   is
      Ref : constant Artifact_Maps.Constant_Reference_Type :=
              Self.Actual.Graph.Artifacts.Constant_Reference (Position.C);
   begin
      return (Element => Ref.Element.all'Unchecked_Access,
              Ref     => Ref);
   end Iter_Constant_Reference;

   --------------------
   -- Iter_Reference --
   --------------------

   function Iter_Reference
     (Self     : access Iterator;
      Position : Cursor) return Reference_Type
   is
      Ref : constant Artifact_Maps.Reference_Type :=
              Self.Actual.Graph.Artifacts.Reference (Position.C);
   begin
      return (Element => Ref.Element.all'Unchecked_Access,
              Ref     => Ref);
   end Iter_Reference;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self     : Artifact_Iterator;
      Position : Cursor) return Cursor
   is
      Res : Cursor := Position;
   begin
      case Res.Kind is
         when Global_List =>
            loop
               Artifact_Maps.Next (Res.C);
               exit when Self.Class = No_Artifact_Class
                 or else not Artifact_Maps.Has_Element (Res.C)
                 or else Artifact_Ids.Class (Artifact_Maps.Key (Res.C)) =
                           Self.Class;
            end loop;

         when Predecessors | Successors =>
            Artifact_Sets.Next (Res.C_Art (Res.Current));

            if not Artifact_Sets.Has_Element (Res.C_Art (Res.Current))
              and then Res.Current /= Dependency_Kind'Last
            then
               Res.Current := Dependency_Kind'Succ (Res.Current);
            end if;

            if Artifact_Sets.Has_Element (Res.C_Art (Res.Current)) then
               Res.C := Self.Graph.Artifacts.Find
                 (Artifact_Sets.Element (Res.C_Art (Res.Current)));
            else
               Res.C := Artifact_Maps.No_Element;
            end if;
      end case;

      return Res;
   end Next;

   ------------------
   -- Predecessors --
   ------------------

   function Predecessors
     (Self     : Object;
      Artifact : Artifact_Ids.Artifact_Id)
      return Iterator'Class is
   begin
      return Iterator'
        (Kind  => Predecessors,
         Actual => (Kind  => Predecessors,
                    Graph => Self'Unrestricted_Access,
                    Pos   => Self.Predecessors.Find (Artifact)));
   end Predecessors;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Self : access Object; Position : Cursor) return Reference_Type
   is
      Ref : constant Artifact_Maps.Reference_Type :=
              Self.Artifacts.Reference (Position.C);
   begin
      return (Element => Ref.Element.all'Unchecked_Access,
              Ref     => Ref);
   end Reference;

   function Reference
     (Self : access Object; Id : Artifact_Id) return Reference_Type
   is
      Ref : constant Artifact_Maps.Reference_Type :=
              Self.Artifacts.Reference (Id);
   begin
      return (Element => Ref.Element.all'Unchecked_Access,
              Ref     => Ref);
   end Reference;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Self     : in out Object;
      Option   : Source_Info_Option;
      Messages : out GPR2.Log.Object) is
   begin
      Self.Src_Option := Option;

      Self.On_Hold := True;
      Self.Current_Action := No_Action_Class;

      --  Refresh each tree's views

      for V of Self.Tree.Ordered_Views loop
         if V.Kind in With_Object_Dir_Kind then
            View_Tables.Check_Source_Lists
              (View_Tables.Get_Data (Self.Self, V), Messages);
         end if;
      end loop;

      for V of Self.Tree.Ordered_Views loop
         if V.Kind in With_Object_Dir_Kind
           and then (Self.With_RTS or else V.Id /= View_Ids.Runtime_View_Id)
         then
            View_Tables.Refresh
              (View_Tables.Get_Data (Self.Self, V), Messages);
         end if;
      end loop;

      --  Do a set of checks for tree/view validity/errors

      for V of Self.Tree.Ordered_Views loop
         if V.Kind in With_Source_Dirs_Kind
           and then not V.Is_Runtime
         then
            --  Check languages

            declare
               SF   : constant Project.Attribute.Object :=
                        V.Attribute (PRA.Source_Files);
               V_Db : constant GPR2.Build.View_Tables.View_Data_Ref :=
                        View_Tables.Get_Data (Self.Self, V);
            begin
               if not SF.Is_Defined
                 or else not SF.Values.Is_Empty
               then
                  for L of V.Languages loop
                     declare
                        Lang : constant Language_Id := +Name_Type (L.Text);
                     begin
                        if not V_Db.Langs_Usage.Contains (Lang)
                          or else V_Db.Langs_Usage (Lang) = 0
                        then
                           Messages.Append
                             (Message.Create
                                (Message.Warning,
                                 "there are no sources of language """ & L.Text
                                 & """ in this project",
                                 L));
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end if;
      end loop;

      for V of Self.Tree.Namespace_Root_Projects loop
         V.Check_Mains (Messages);
      end loop;

      if Self.Src_Option >= Sources_Units then
         for V of Self.Tree.Namespace_Root_Projects loop
            if V.Kind in With_Object_Dir_Kind then
               declare
                  V_Db : constant View_Tables.View_Data_Ref :=
                           View_Tables.Get_Data (Self.Self, V);
               begin
                  for U of V_Db.CUs loop
                     U.Check_Name_Validity (Messages);
                  end loop;
               end;
            end if;
         end loop;

         for V of Self.Tree.Ordered_Views loop
            if V.Kind in With_Object_Dir_Kind then
               declare
                  use GPR2.Containers;
                  V_Db : constant View_Tables.View_Data_Ref :=
                           View_Tables.Get_Data (Self.Self, V);
               begin
                  for C in V.Interface_Units.Iterate loop
                     if not V_Db.Own_CUs.Contains
                       (Unit_Name_To_Sloc.Key (C))
                     then
                        Messages.Append
                          (Message.Create
                             (Message.Error,
                              "source for interface unit '" &
                                String (Unit_Name_To_Sloc.Key (C)) &
                                "' not found",
                              Unit_Name_To_Sloc.Element (C)));
                     end if;
                  end loop;

                  for C in V.Interface_Sources.Iterate loop
                     if not V_Db.Sources.Contains
                       (Source_Path_To_Sloc.Key (C))
                     then
                        Messages.Append
                          (Message.Create
                             (Message.Error,
                              "source for interface '" &
                                String (Source_Path_To_Sloc.Key (C)) &
                                "' not found",
                              Source_Path_To_Sloc.Element (C)));
                     end if;
                  end loop;
               end;
            end if;
         end loop;
      end if;

      if Option >= Sources_Units_Artifacts then
         Self.Release;
      end if;
   end Refresh;

   ---------------------
   -- Register_Action --
   ---------------------

   procedure Register_Action
     (Self   : access Object;
      Action : Actions.Object'Class)
   is
      Input : constant Artifact_Class := Action.Inputs;
      C     : Artifact_Actions_Maps.Cursor;
      Done  : Boolean;
   begin
      Self.Actions_List.Insert (Action.Class, Action);
      Self.Todo_List.Insert (Input, Action_Sets.Empty_Set, C, Done);
      Self.Todo_List (C).Insert (Action);
   end Register_Action;

   -------------
   -- Release --
   -------------

   procedure Release (Self : access Object) is
   begin
      loop
         declare
            To_Remove : constant Artifact_Sets.Set :=
                          Self.To_Remove;
         begin
            exit when To_Remove.Is_Empty;

            Self.To_Remove.Clear;

            for C of To_Remove loop
               Self.Remove_Artifact (C);
            end loop;
         end;
      end loop;

      loop
         declare
            Inserted : constant Artifact_Class_Artifact_Set_Maps.Map :=
                         Self.New_Artifacts;
         begin
            exit when Inserted.Is_Empty;

            Self.New_Artifacts.Clear;

            for C in Inserted.Iterate loop
               declare
                  Class   : constant Artifact_Class :=
                              Artifact_Class_Artifact_Set_Maps.Key (C);
                  C_Action : constant Artifact_Actions_Maps.Cursor :=
                               Self.Todo_List.Find (Class);
               begin
                  if Artifact_Actions_Maps.Has_Element (C_Action) then
                     for Action of Self.Todo_List.Reference (C_Action) loop
                        Self.Current_Action := Action.Class;

                        for Artifact of Inserted.Constant_Reference (C) loop
                           Action.Fill (Self, Artifact);
                        end loop;
                     end loop;
                  end if;
               end;
            end loop;
         end;
      end loop;

      --  TODO: checks and update the TODO list once we have it

      Self.On_Hold := False;
   end Release;

   ---------------------
   -- Remove_Artifact --
   ---------------------

   procedure Remove_Artifact
     (Self     : access Object;
      Artifact : Artifact_Ids.Artifact_Id)
   is
      C : Artifact_Dependency_Maps.Cursor;
   begin
      C := Self.Predecessors.Find (Artifact);

      if Artifact_Dependency_Maps.Has_Element (C) then
         for Kind in Dependency_Kind loop
            for C2 in
              Self.Predecessors.Reference (C).Inputs (Kind).Iterate
            loop
               Self.Successors
                 (Artifact_Sets.Element (C2)).Inputs (Kind).Delete (Artifact);
            end loop;
         end loop;

         Self.Predecessors.Delete (C);
      end if;

      C := Self.Successors.Find (Artifact);

      if Artifact_Dependency_Maps.Has_Element (C) then
         for Kind in Dependency_Kind loop
            for C2 in Self.Successors.Reference (C).Inputs (Kind).Iterate loop
               Self.Predecessors
                 (Artifact_Sets.Element (C2)).Inputs (Kind).Delete (Artifact);

               if Kind = Explicit
                 and then Self.Predecessors
                            (Artifact_Sets.Element (C2)).Inputs (Kind).Is_Empty
               then
                  Self.To_Remove.Include (Artifact_Sets.Element (C2));
               end if;
            end loop;
         end loop;

         Self.Successors.Delete (C);
      end if;

      Self.Artifacts.Delete (Artifact);
   end Remove_Artifact;

   ----------------
   -- Successors --
   ----------------

   function Successors
     (Self     : Object;
      Artifact : Artifact_Ids.Artifact_Id)
      return Iterator'Class is
   begin
      return Iterator'
        (Kind   => Successors,
         Actual => (Kind  => Successors,
                    Graph => Self'Unrestricted_Access,
                    Pos   => Self.Successors.Find (Artifact)));
   end Successors;

   ----------
   -- Tree --
   ----------

   function Tree (Self : Object) return access GPR2.Project.Tree.Object is
     (Self.Tree);

   ------------
   -- Unload --
   ------------

   procedure Unload (Self : in out Object) is
   begin
      Self.Build_Dbs.Clear;
      Self.Tree := null;
      Self.Self := null;
      Self.Src_Option := No_Source;
      Self.With_RTS   := False;
   end Unload;

   -------------------
   -- View_Database --
   -------------------

   function View_Database
     (Self : Object; View : GPR2.Project.View.Object)
      return Build.View_Db.Object is
   begin
      return Self.Build_Dbs.Element (View.Id);
   end View_Database;

end GPR2.Build.Tree_Db;
