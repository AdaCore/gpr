--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.DAG is

   ------------------
   -- Add_Artifact --
   ------------------

   procedure Add_Artifact
     (Self     : access Object;
      Artifact : Artifacts.Object'Class)
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
      C   : Cursor (Self.Kind);
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

         when Pred_Succ =>
            if not Artifact_Artifact_Set_Maps.Has_Element (Self.Pos) then
               return No_Element;
            else
               C.C_A := Artifact_Artifact_Set_Maps.Element (Self.Pos).First;

               if Artifact_Sets.Has_Element (C.C_A) then
                  C.C := Self.Graph.Artifacts.Find
                    (Artifact_Sets.Element (C.C_A));
               else
                  return No_Element;
               end if;
            end if;
      end case;

      return C;
   end First;

   ----------
   -- Hold --
   ----------

   procedure Hold (Self : access Object) is
   begin
      Self.On_Hold := True;
   end Hold;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self  : Object;
      Class : Artifact_Class := No_Artifact_Class)
      return Artifact_Iterators.Forward_Iterator'Class
   is
   begin
      return Artifact_Iterator'
               (Kind  => Global_List,
                Graph => Self'Unrestricted_Access,
                Class => Class);
   end Iterate;

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

         when Pred_Succ =>
            Artifact_Sets.Next (Res.C_A);

            if Artifact_Sets.Has_Element (Res.C_A) then
               Res.C := Self.Graph.Artifacts.Find
                 (Artifact_Sets.Element (Res.C_A));
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
      return Artifact_Iterators.Forward_Iterator'Class is
   begin
      return Artifact_Iterator'
               (Kind  => Pred_Succ,
                Graph => Self'Unrestricted_Access,
                Pos   => Self.Predecessors.Find (Artifact));
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
      Self.Actions_List.Insert (Input, Action_Sets.Empty_Set, C, Done);
      Self.Actions_List (C).Insert (Action);
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
                               Self.Actions_List.Find (Class);
               begin
                  if Artifact_Actions_Maps.Has_Element (C_Action) then
                     for Action of Self.Actions_List.Reference (C_Action) loop
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
      C    : Artifact_Artifact_Set_Maps.Cursor;
   begin
      C := Self.Predecessors.Find (Artifact);

      if Artifact_Artifact_Set_Maps.Has_Element (C) then
         for C2 in Self.Predecessors.Reference (C).Iterate loop
            Self.Successors (Artifact_Sets.Element (C2)).Delete (Artifact);
         end loop;

         Self.Predecessors.Delete (C);
      end if;

      C := Self.Successors.Find (Artifact);
      if Artifact_Artifact_Set_Maps.Has_Element (C) then
         for C2 in Self.Successors.Reference (C).Iterate loop
            Self.Predecessors (Artifact_Sets.Element (C2)).Delete (Artifact);

            if Self.Predecessors (Artifact_Sets.Element (C2)).Is_Empty then
               Self.To_Remove.Include (Artifact_Sets.Element (C2));
            end if;
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
      return Artifact_Iterators.Forward_Iterator'Class is
   begin
      return Artifact_Iterator'
               (Kind  => Pred_Succ,
                Graph => Self'Unrestricted_Access,
                Pos   => Self.Successors.Find (Artifact));
   end Successors;

   ---------------------
   -- Update_Artifact --
   ---------------------

   procedure Update_Artifact
     (Self        : access Object;
      Artifact    : Artifact_Ids.Artifact_Id;
      Predecessor : Artifact_Ids.Artifact_Id)
   is
      C    : Artifact_Artifact_Set_Maps.Cursor;
      Done : Boolean;
   begin
      Self.Predecessors.Insert
        (Artifact, Artifact_Sets.Empty_Set, C, Done);
      Self.Predecessors (C).Include (Predecessor);
      Self.Successors.Insert
        (Predecessor, Artifact_Sets.Empty_Set, C, Done);
      Self.Successors (C).Include (Artifact);
      Self.Sort_Valid := False;
   end Update_Artifact;

end GPR2.Build.DAG;
