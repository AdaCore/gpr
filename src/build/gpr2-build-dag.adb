--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.DAG is

   procedure Add_Dependency
     (Self        : access Object;
      Artifact    : Artifact_Ids.Artifact_Id;
      Predecessor : Artifact_Ids.Artifact_Id;
      Kind        : Dependency_Kind);

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

   ----------
   -- Hold --
   ----------

   procedure Hold (Self : access Object) is
   begin
      Self.On_Hold := True;
      Self.Current_Action := No_Action_Class;
   end Hold;

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

end GPR2.Build.DAG;
