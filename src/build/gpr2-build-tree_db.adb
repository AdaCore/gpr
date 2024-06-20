--
--  Copyright (C) 2022-2024, AdaCore
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
with GPR2.Tree_Internal;
with GPR2.View_Ids.Set;

package body GPR2.Build.Tree_Db is

   package PRA renames GPR2.Project.Registry.Attribute;

   use type GPR2.View_Ids.View_Id;

   type Artifact_Internal_Iterator is limited new
     Artifact_Iterators.Forward_Iterator with record
      Db     : access Object;
      Kind   : Artifact_List_Kind;
      Action : Action_Maps.Cursor;
   end record;

   overriding function First
     (Iter : Artifact_Internal_Iterator) return Artifact_Cursor;

   overriding function Next
     (Iter     : Artifact_Internal_Iterator;
      Position : Artifact_Cursor) return Artifact_Cursor;

   function Artifact_Iterate
     (List : Artifacts_List) return Artifact_Iterators.Forward_Iterator'Class
   is (Artifact_Internal_Iterator'
         (Db     => List.Db,
          Kind   => List.Kind,
          Action => (if List.Kind = Global_List
                     then Action_Maps.No_Element
                     else List.Action)));

   type Action_Internal_Iterator is limited new
     Action_Iterators.Forward_Iterator with record
      Db       : access Object;
      Kind     : Action_List_Kind;
      Artifact : Artifact_Sets.Cursor;
   end record;

   overriding function First
     (Iter : Action_Internal_Iterator) return Action_Cursor;

   overriding function Next
     (Iter : Action_Internal_Iterator;
      Position : Action_Cursor) return Action_Cursor;

   function Action_Iterate
     (List : Actions_List) return Action_Iterators.Forward_Iterator'Class
   is (Action_Internal_Iterator'
         (Db       => List.Db,
          Kind     => List.Kind,
          Artifact => (if List.Kind = Global_List
                       then Artifact_Sets.No_Element
                       else List.Artifact)));

   ----------------------------
   -- Action_Id_To_Reference --
   ----------------------------

   function Action_Id_To_Reference
     (Self : in out Object;
      Id   : Actions.Action_Id'Class) return Action_Reference_Type
   is
      Ref : constant Action_Maps.Reference_Type := Self.Actions.Reference (Id);
   begin
      return (Element => Ref.Element.all'Unchecked_Access, Ref => Ref);
   end Action_Id_To_Reference;

   ----------------------
   -- Action_Reference --
   ----------------------

   function Action_Reference
     (Iterator : access Actions_List;
      Pos      : Action_Cursor) return Action_Reference_Type
   is
      Ref : constant Action_Maps.Reference_Type :=
              Iterator.Db.Actions.Reference (Pos.Pos);
   begin
      return (Element => Ref.Element.all'Unchecked_Access, Ref => Ref);
   end Action_Reference;

   ----------------
   -- Add_Action --
   ----------------

   procedure Add_Action
     (Self     : in out Object;
      Action   : in out Actions.Object'Class;
      Messages : in out GPR2.Log.Object)
   is
      Curs : Action_Maps.Cursor;
      Done : Boolean;
   begin
      Self.Actions.Insert (Action.UID, Action, Curs, Done);
      pragma Assert (Done, "duplicated action detected");
      Self.Implicit_Inputs.Insert (Action.UID, Artifact_Sets.Empty_Set);
      Self.Inputs.Insert (Action.UID, Artifact_Sets.Empty_Set);
      Self.Outputs.Insert (Action.UID, Artifact_Sets.Empty_Set);

      Self.Actions.Reference (Curs).Attach (Self);
      Self.Actions.Reference (Curs).On_Tree_Insertion (Self, Messages);
      Self.Actions.Reference (Curs).Compare_Signature (Messages);

      --  `Attach` modifies the Tree field of the action. The provided action
      --  needs to be updated, as some action subprograms may require the
      --  internal tree field.

      Action := Self.Actions.Reference (Curs);
   end Add_Action;

   ------------------
   -- Add_Artifact --
   ------------------

   procedure Add_Artifact
     (Self     : in out Object;
      Artifact : Artifacts.Object'Class)
   is
      Curs : Artifact_Sets.Cursor;
      Done : Boolean;
   begin
      Self.Artifacts.Insert (Artifact, Curs, Done);

      if Done then
         Self.Successors.Insert (Artifact, Action_Sets.Empty_Set);
      end if;
   end Add_Artifact;

   ---------------
   -- Add_Input --
   ---------------

   procedure Add_Input
     (Self     : in out Object;
      Action   : Actions.Action_Id'Class;
      Artifact : Artifacts.Object'Class;
      Explicit : Boolean) is
   begin
      Self.Add_Artifact (Artifact);

      if Explicit then
         Self.Inputs.Reference (Action).Include (Artifact);
      else
         Self.Implicit_Inputs.Reference (Action).Include (Artifact);
      end if;

      Self.Successors.Reference (Artifact).Include (Action);
   end Add_Input;

   ----------------
   -- Add_Output --
   ----------------

   procedure Add_Output
     (Self     : in out Object;
      Action   : Actions.Action_Id'Class;
      Artifact : Artifacts.Object'Class;
      Messages : in out GPR2.Log.Object)
   is
      use type Actions.Action_Id;
      Pred     : Artifact_Action_Maps.Cursor;

   begin
      Self.Add_Artifact (Artifact);

      Pred := Self.Predecessor.Find (Artifact);

      if Artifact_Action_Maps.Has_Element (Pred) then
         if Self.Predecessor (Pred) /= Action then
            --  Two actions produce the same output, raise an error
            Messages.Append
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  '"' & Action.Image & """ and """ &
                    Self.Predecessor (Artifact).Image &
                    """ produce the same output """ &
                    Artifact.Image & '"',
                  Artifact.SLOC));

            return;
         end if;
      else
         Self.Predecessor.Insert (Artifact, Action);
      end if;

      Self.Outputs.Reference (Action).Include (Artifact);
   end Add_Output;

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

   -------------------------------
   -- Constant_Action_Reference --
   -------------------------------

   function Constant_Action_Reference
     (Iterator : aliased Actions_List;
      Pos      : Action_Cursor) return Constant_Action_Reference_Type
   is
      Ref : constant Action_Maps.Constant_Reference_Type :=
              Iterator.Db.Actions.Constant_Reference (Pos.Pos);
   begin
      return (Element => Ref.Element.all'Unchecked_Access, Ref => Ref);
   end Constant_Action_Reference;

   ---------------------------------
   -- Constant_Artifact_Reference --
   ---------------------------------

   function Constant_Artifact_Reference
     (Iterator : aliased Artifacts_List;
      Pos      : Artifact_Cursor) return Constant_Artifact_Reference_Type
   is
      Ref : constant Artifact_Sets.Constant_Reference_Type :=
              (case Pos.Current is
               when Global_List     =>
                  Iterator.Db.Artifacts.Constant_Reference (Pos.Pos),
               when Implicit_Inputs =>
                  Iterator.Db.Implicit_Inputs.Constant_Reference
                    (Pos.Map_Pos).Constant_Reference (Pos.Pos),
               when Explicit_Inputs =>
                  Iterator.Db.Inputs.Constant_Reference
                    (Pos.Map_Pos).Constant_Reference (Pos.Pos),
               when Outputs         =>
                  Iterator.Db.Outputs.Constant_Reference
                    (Pos.Map_Pos).Constant_Reference (Pos.Pos),
               when others          =>
                    raise Program_Error with "Wrong kind of cursor");
   begin
      return (Element => Ref.Element.all'Unchecked_Access, Ref => Ref);
   end Constant_Artifact_Reference;

   ------------
   -- Create --
   ------------

   procedure Create
     (Self                 : in out Object;
      Tree                 : GPR2.Tree_Internal.Object;
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

   ----------------------
   -- DB_Filename_Path --
   ----------------------

   function Db_Filename_Path
     (Self   : in out Object;
      Action : Actions.Action_Id'Class) return Path_Name.Object
   is
      Curs : constant Action_Maps.Cursor := Self.Actions.Find (Action);
   begin
      return (GPR2.Path_Name.Create_File
              (Self.Actions.Reference (Curs).UID.Db_Filename,
               Self.Actions.Reference (Curs).View.Object_Directory.Value)
             );
   end Db_Filename_Path;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Self   : in out Object;
      Action : Actions.Action_Id'Class)
   is
      Curs : constant Action_Maps.Cursor := Self.Actions.Find (Action);
   begin
      Self.Actions.Reference (Curs).Compute_Signature;
   end Execute;

   -----------
   -- First --
   -----------

   overriding function First
     (Iter : Artifact_Internal_Iterator) return Artifact_Cursor
   is
      Res     : Artifact_Cursor;
      Map_Pos : Action_Artifacts_Maps.Cursor;

   begin
      if Iter.Db.Artifacts.Is_Empty then
         return No_Artifact_Element;
      end if;

      case Iter.Kind is
         when Global_List =>
            return (Pos     => Iter.Db.Artifacts.First,
                    Map_Pos => Action_Artifacts_Maps.No_Element,
                    Current => Iter.Kind);
         when Explicit_Inputs | Inputs =>
            declare
               Id      : constant Actions.Action_Id'Class :=
                           Action_Maps.Key (Iter.Action);
            begin
               Map_Pos := Iter.Db.Inputs.Find (Id);
               Res :=
                 (Pos     => Iter.Db.Inputs.Constant_Reference (Map_Pos).First,
                  Map_Pos => Map_Pos,
                  Current => Explicit_Inputs);

               if not Artifact_Sets.Has_Element (Res.Pos)
                 and then Iter.Kind = Inputs
               then
                  Map_Pos := Iter.Db.Implicit_Inputs.Find (Id);
                  Res :=
                    (Pos     =>
                       Iter.Db.Implicit_Inputs.Constant_Reference
                         (Map_Pos).First,
                     Map_Pos => Map_Pos,
                     Current => Implicit_Inputs);
               end if;
            end;

         when Implicit_Inputs =>
            Map_Pos :=
              Iter.Db.Implicit_Inputs.Find (Action_Maps.Key (Iter.Action));
            Res :=
              (Pos    => Iter.Db.Implicit_Inputs.Constant_Reference
                           (Map_Pos).First,
               Map_Pos => Map_Pos,
               Current => Iter.Kind);

         when Outputs =>
            Map_Pos := Iter.Db.Outputs.Find (Action_Maps.Key (Iter.Action));
            Res :=
              (Pos     => Iter.Db.Outputs.Constant_Reference (Map_Pos).First,
               Map_Pos => Map_Pos,
               Current => Iter.Kind);

      end case;

      if not Artifact_Sets.Has_Element (Res.Pos) then
         return No_Artifact_Element;
      else
         return Res;
      end if;
   end First;

   -----------
   -- First --
   -----------

   overriding function First
     (Iter : Action_Internal_Iterator) return Action_Cursor
   is
      Res : Action_Cursor;
   begin
      if Iter.Db.Actions.Is_Empty then
         return No_Action_Element;
      end if;

      case Iter.Kind is
         when Global_List =>
            return (Pos     => Iter.Db.Actions.First,
                    Set_Pos => Action_Sets.No_Element);

         when Successors =>
            declare
               Artifact : constant Artifacts.Object'Class :=
                            Iter.Db.Artifacts.Element (Iter.Artifact);
            begin
               Res.Set_Pos :=
                 Iter.Db.Successors.Constant_Reference (Artifact).First;

               if not Action_Sets.Has_Element (Res.Set_Pos) then
                  return No_Action_Element;
               else
                  Res.Pos :=
                    Iter.Db.Actions.Find (Action_Sets.Element (Res.Set_Pos));
                  return Res;
               end if;
            end;
      end case;
   end First;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter     : Artifact_Internal_Iterator;
      Position : Artifact_Cursor) return Artifact_Cursor
   is
      Res : Artifact_Cursor := Position;
   begin
      Artifact_Sets.Next (Res.Pos);

      if not Artifact_Sets.Has_Element (Res.Pos)
        and then Iter.Kind = Inputs
        and then Res.Current = Explicit_Inputs
      then
         Res.Current := Implicit_Inputs;
         Res.Map_Pos :=
           Iter.Db.Implicit_Inputs.Find (Action_Maps.Key (Iter.Action));
         Res.Pos     :=
           Iter.Db.Implicit_Inputs.Constant_Reference (Res.Map_Pos).First;
      end if;

      if not Artifact_Sets.Has_Element (Res.Pos) then
         return No_Artifact_Element;
      end if;

      return Res;
   end Next;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Action_Internal_Iterator;
      Position : Action_Cursor) return Action_Cursor
   is
      Res : Action_Cursor := Position;
   begin
      case Iter.Kind is
         when Global_List =>
            Action_Maps.Next (Res.Pos);

         when Successors =>
            Action_Sets.Next (Res.Set_Pos);

            if not Action_Sets.Has_Element (Res.Set_Pos) then
               return No_Action_Element;
            else
               Res.Pos :=
                 Iter.Db.Actions.Find (Action_Sets.Element (Res.Set_Pos));
            end if;
      end case;

      return Res;
   end Next;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Self     : in out Object;
      Option   : Source_Info_Option;
      Messages : out GPR2.Log.Object) is
   begin
      Self.Src_Option := Option;

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
   end Refresh;

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

begin

   GPR2.Tree_Internal.Init_Tree_Database := Create'Access;

end GPR2.Build.Tree_Db;
