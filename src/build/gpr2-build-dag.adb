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
     (Self     : access DAG;
      Artifact : Artifacts.Object'Class) is
   begin
      Self.Artifacts.Insert (Artifact.Id, Artifact);
   end Add_Artifact;

   ---------------------
   -- Register_Action --
   ---------------------

   procedure Register_Action
     (Self   : access DAG;
      Action : Actions.Object'Class)
   is
      Input : constant Artifact_Class := Action.Inputs;
      C     : Artifact_Actions_Maps.Cursor;
      Done  : Boolean;
   begin
      Self.Actions_List.Insert (Input, Action_Sets.Empty_Set, C, Done);
      Self.Actions_List (C).Insert (Action);
   end Register_Action;

   ---------------------
   -- Remove_Artifact --
   ---------------------

   procedure Remove_Artifact
     (Self     : access DAG;
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
         end loop;

         Self.Successors.Delete (C);
      end if;

      Self.Artifacts.Delete (Artifact);
   end Remove_Artifact;

   ---------------------
   -- Update_Artifact --
   ---------------------

   procedure Update_Artifact
     (Self        : access DAG;
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
