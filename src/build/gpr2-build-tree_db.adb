--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.FSUtil;
pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);

with GPR2.Build.View_Tables;
with GPR2.Containers;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Tree_Internal;
with GPR2.View_Ids.Set;
with GNATCOLL.Directed_Graph; use GNATCOLL.Directed_Graph;

package body GPR2.Build.Tree_Db is

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   procedure Create_View_Dbs  (Self : in out Object);

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

   --------------------
   -- Action_Iterate --
   --------------------

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
     (Iterator : aliased in out Actions_List;
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

   function Add_Action
     (Self     : in out Object;
      Action   : in out Actions.Object'Class) return Boolean
   is
      Curs : Action_Maps.Cursor;
      Done : Boolean;
      Node : GNATCOLL.Directed_Graph.Node_Id;
   begin
      Action.Attach (Self);
      Self.Actions.Insert (Action.UID, Action, Curs, Done);

      if not Done then
         Action := Self.Actions.Reference (Curs);
         return True;
      end if;

      Self.New_Actions.Include (Action.UID);

      Self.Implicit_Inputs.Insert (Action.UID, Artifact_Sets.Empty_Set);
      Self.Inputs.Insert (Action.UID, Artifact_Sets.Empty_Set);
      Self.Outputs.Insert (Action.UID, Artifact_Sets.Empty_Set);

      if not Action.On_Tree_Insertion (Self) then
         return False;
      end if;

      Action := Self.Actions.Reference (Curs);

      if Self.Executing then
         --  Adding a new action while executing the graph: we need to
         --  amend the Execution context
         Node := Self.Exec_Ctxt.Graph.Add_Node;
         Self.Exec_Ctxt.Actions.Insert (Node, Action.UID);
         Self.Exec_Ctxt.Nodes.Insert (Action.UID, Node);
      end if;

      return True;
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
      Explicit : Boolean)
   is
      Pred : Artifact_Action_Maps.Cursor;
   begin
      Self.Add_Artifact (Artifact);

      if Explicit then
         Self.Inputs.Reference (Action).Include (Artifact);
      elsif not Self.Inputs.Reference (Action).Contains (Artifact) then
         Self.Implicit_Inputs.Reference (Action).Include (Artifact);
      else
         return;
      end if;

      Self.Successors.Reference (Artifact).Include (Action);

      if Self.Executing then
         --  need to amend the execution context dependencies
         Pred := Self.Predecessor.Find (Artifact);

         if Artifact_Action_Maps.Has_Element (Pred) then
            Self.Exec_Ctxt.Graph.Add_Predecessor
              (Node        => Self.Exec_Ctxt.Nodes (Action),
               Predecessor =>
                 Self.Exec_Ctxt.Nodes (Artifact_Action_Maps.Element (Pred)));
         end if;
      end if;
   end Add_Input;

   ----------------
   -- Add_Output --
   ----------------

   function Add_Output
     (Self     : in out Object;
      Action   : Actions.Action_Id'Class;
      Artifact : Artifacts.Object'Class) return Boolean
   is
      use type Actions.Action_Id;
      Pred     : Artifact_Action_Maps.Cursor;

   begin
      Self.Add_Artifact (Artifact);

      Pred := Self.Predecessor.Find (Artifact);

      if Artifact_Action_Maps.Has_Element (Pred) then
         if Self.Predecessor (Pred) /= Action then
            --  Two actions produce the same output, raise an error
            Self.Tree.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  '"' & Action.Image & """ and """ &
                    Self.Predecessor (Artifact).Image &
                    """ produce the same output """ &
                    Artifact.Image & '"',
                  Artifact.SLOC));
            return False;
         end if;
      else
         Self.Predecessor.Insert (Artifact, Action);
      end if;

      Self.Outputs.Reference (Action).Include (Artifact);

      if Self.Executing then
         for Id of Self.Successors (Artifact) loop
            Self.Exec_Ctxt.Graph.Add_Predecessor
              (Node        => Self.Exec_Ctxt.Nodes (Id),
               Predecessor => Self.Exec_Ctxt.Nodes (Action));
         end loop;
      end if;

      return True;
   end Add_Output;

   ----------------
   -- Check_Tree --
   ----------------

   procedure Check_Tree (Self : in out Object) is
      To_Remove : GPR2.View_Ids.Set.Set;
   begin
      --  Check for new views

      for V of Self.Tree.Ordered_Views loop
         if not Self.Build_Dbs.Contains (V.Id)
           and then V.Kind in GPR2.Build.View_Tables.With_View_Db
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

   ----------------------
   -- Clear_Temp_Files --
   ----------------------

   procedure Clear_Temp_Files (Self : Object) is
   begin
      for V_Db of Self.Build_Dbs loop
         declare
            Data_Ref : constant View_Tables.View_Data_Ref :=
                         View_Tables.Get_Ref (V_Db);
            Dead     : Boolean with Unreferenced;
         begin
            for Temp of Data_Ref.Temp_Files loop
               Dead := GNATCOLL.OS.FSUtil.Remove_File (String (Temp));
            end loop;

            Data_Ref.Temp_Files.Clear;
         end;
      end loop;
   end Clear_Temp_Files;

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
                  raise Internal_Error with "Wrong kind of cursor");
   begin
      return (Element => Ref.Element.all'Unchecked_Access, Ref => Ref);
   end Constant_Artifact_Reference;

   ------------
   -- Create --
   ------------

   procedure Create
     (Self : in out Object;
      Tree : GPR2.Tree_Internal.Object)
   is
   begin
      if Self.Self = null then
         Self.Self := Self'Unrestricted_Access;
         Self.Tree := Tree.Reference;
      end if;

      Self.Create_View_Dbs;
   end Create;

   ---------------------
   -- Create_View_Dbs --
   ---------------------

   procedure Create_View_Dbs  (Self : in out Object) is
      Db_Inst : View_Db.Object;

   begin
      for V of Self.Tree.Ordered_Views loop
         if V.Kind in GPR2.Build.View_Tables.With_View_Db then
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
   end Create_View_Dbs;

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
               Self.Actions.Reference (Curs).View.Object_Directory.Value));
   end Db_Filename_Path;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Self    : in out Object;
      PM      : in out GPR2.Build.Process_Manager.Object'Class;
      Options : GPR2.Build.Process_Manager.PM_Options)
   is
      Node : GNATCOLL.Directed_Graph.Node_Id;
      Pred : Artifact_Action_Maps.Cursor;

   begin
      --  Populate the DAG used for the execution

      --  First ensure all actions correspond to a node in the DAG

      for Action of Self.Actions loop
         Node := Self.Exec_Ctxt.Graph.Add_Node;
         Self.Exec_Ctxt.Actions.Insert (Node, Action.UID);
         Self.Exec_Ctxt.Nodes.Insert (Action.UID, Node);
      end loop;

      --  Now propagate the dependencies

      for Action of Self.Actions loop
         for Input of Self.Inputs (Action.UID).Union
           (Self.Implicit_Inputs (Action.UID))
         loop
            --  Find the action that generated this input
            Pred := Self.Predecessor.Find (Input);

            if Artifact_Action_Maps.Has_Element (Pred) then
               Self.Exec_Ctxt.Graph.Add_Predecessor
                 (Node        => Self.Exec_Ctxt.Nodes (Action.UID),
                  Predecessor =>
                    Self.Exec_Ctxt.Nodes
                      (Artifact_Action_Maps.Element (Pred)));
            end if;
         end loop;

         for Output of Self.Outputs (Action.UID) loop
            for Suc of Self.Successors (Output) loop
               Self.Exec_Ctxt.Graph.Add_Predecessor
                 (Node        => Self.Exec_Ctxt.Nodes (Suc),
                  Predecessor => Self.Exec_Ctxt.Nodes (Action.UID));
            end loop;
         end loop;
      end loop;

      Self.Executing := True;
      PM.Execute
        (Self.Self,
         Context => Self.Exec_Ctxt'Access,
         Options => Options);

      Self.Executing := False;
      Self.Exec_Ctxt.Graph.Clear;
      Self.Exec_Ctxt.Actions.Clear;
      Self.Exec_Ctxt.Nodes.Clear;
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

   -----------------------------
   -- Get_Or_Create_Temp_File --
   -----------------------------

   function Get_Or_Create_Temp_File
     (Self     : Object;
      For_View : GPR2.Project.View.Object;
      Purpose  : Simple_Name) return Temp_File
   is
      Data : constant View_Tables.View_Data_Ref :=
               View_Tables.Get_Ref (Self.Build_Dbs (For_View.Id));
      C    : View_Tables.Temp_File_Maps.Cursor;
      Dest : Path_Name.Object;
      Done : Boolean;

   begin
      C := Data.Temp_Files.Find (Purpose);

      if not View_Tables.Temp_File_Maps.Has_Element (C) then
         declare
            BN : constant Simple_Name :=
                   "." & Purpose & ".tmp";
         begin
            Dest := For_View.Object_Directory.Compose (BN);
            Data.Temp_Files.Insert (Purpose, Dest.Value, C, Done);

            pragma Assert (Done);

            return
              (Path_Len => Dest.Value'Length,
               FD       => GNATCOLL.OS.FS.Open
                             (Dest.String_Value,
                              GNATCOLL.OS.FS.Write_Mode),
               Path     => Dest.Value);
         end;
      else
         declare
            Path : Filename_Type renames
                     View_Tables.Temp_File_Maps.Element (C);
         begin
            return
              (Path_Len => Path'Length,
               FD       => GNATCOLL.OS.FS.Null_FD,
               Path     => Path);
         end;
      end if;
   end Get_Or_Create_Temp_File;

   ---------------------------
   -- Linker_Lib_Dir_Option --
   ---------------------------

   function Linker_Lib_Dir_Option (Self : Object) return Value_Type is
      Attr : GPR2.Project.Attribute.Object;
   begin
      if Length (Self.Linker_Lib_Dir_Opt) = 0 then
         if Self.Tree.Has_Configuration then
            Attr := Self.Tree.Configuration.Corresponding_View.Attribute
              (PRA.Linker_Lib_Dir_Option);
         end if;

         if Attr.Is_Defined then
            Self.Self.Linker_Lib_Dir_Opt := +Attr.Value.Text;
         else
            Self.Self.Linker_Lib_Dir_Opt := To_Unbounded_String ("-L");
         end if;
      end if;

      return -Self.Linker_Lib_Dir_Opt;
   end Linker_Lib_Dir_Option;

   ---------------------
   -- Load_Signatures --
   ---------------------

   procedure Load_Signatures (Self : Object) is
   begin
      for A of Self.Self.Actions loop
         A.Load_Signature;
      end loop;
   end Load_Signatures;

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

   -----------------------
   -- Propagate_Actions --
   -----------------------

   function Propagate_Actions (Self : Object) return Boolean is
      New_Actions : Action_Sets.Set;
   begin
      loop
         if not Self.New_Actions.Is_Empty then
            New_Actions.Union (Self.New_Actions);
            Self.Self.New_Actions.Clear;
         end if;

         exit when New_Actions.Is_Empty;

         declare
            Item : constant Actions.Action_Id'Class :=
                     New_Actions.First_Element;
            Act  : Actions.Object'Class :=
                     Self.Actions.Element (Item);
         begin
            New_Actions.Delete_First;

            if not Act.On_Tree_Propagation then
               return False;
            end if;

            Self.Self.Action_Id_To_Reference (Item) := Act;
         end;
      end loop;

      return True;
   end Propagate_Actions;

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
         if V.Kind in With_Source_Dirs_Kind then
            View_Tables.Check_Source_Lists
              (View_Tables.Get_Data (Self.Self, V), Messages);
         end if;
      end loop;

      for V of Self.Tree.Ordered_Views loop
         if V.Kind in With_Object_Dir_Kind then
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
                        if (not V_Db.Langs_Usage.Contains (Lang)
                            or else V_Db.Langs_Usage (Lang) = 0)
                          and then V.Attribute (PRA.Compiler.Driver,
                                                PAI.Create (Lang)).Is_Defined
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
         if V.Kind in GPR2.Build.View_Tables.With_View_Db then
            V.Check_Mains (Messages);
         end if;
      end loop;

      if Self.Src_Option >= Sources_Units then
         for V of Self.Tree.Namespace_Root_Projects loop
            if V.Kind in GPR2.Build.View_Tables.With_View_Db then
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
            if V.Kind in GPR2.Build.View_Tables.With_View_Db then
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

   -------------------
   -- Remove_Action --
   -------------------

   procedure Remove_Action
     (Self : in out Object;
      Id   : Actions.Action_Id'Class)
   is
      C : Action_Sets.Cursor;
   begin
      Self.Actions.Delete (Id);

      for Input of Self.Inputs (Id) loop
         Self.Successors (Input).Delete (Id);
      end loop;

      for Input of Self.Implicit_Inputs (Id) loop
         Self.Successors (Input).Delete (Id);
      end loop;

      for Output of Self.Outputs (Id) loop
         Self.Predecessor.Delete (Output);
      end loop;

      Self.Implicit_Inputs.Delete (Id);
      Self.Inputs.Delete (Id);
      Self.Outputs.Delete (Id);

      C := Self.New_Actions.Find (Id);
      if Action_Sets.Has_Element (C) then
         Self.New_Actions.Delete (C);
      end if;
   end Remove_Action;

   ---------------------
   -- Remove_Artifact --
   ---------------------

   procedure Remove_Artifact
     (Self     : in out Object;
      Artifact : Artifacts.Object'Class)
   is
      C      : Artifact_Sets.Cursor;
      C_Succ : Artifact_Actions_Maps.Cursor;
      C_Pred : Artifact_Action_Maps.Cursor;

   begin
      C := Self.Artifacts.Find (Artifact);

      if not Artifact_Sets.Has_Element (C) then
         return;
      end if;

      Self.Artifacts.Delete (C);

      C_Succ := Self.Successors.Find (Artifact);

      for Succ of Self.Successors (C_Succ) loop
         C := Self.Inputs (Succ).Find (Artifact);

         if Artifact_Sets.Has_Element (C) then
            Self.Inputs (Succ).Delete (C);
         end if;

         C := Self.Implicit_Inputs (Succ).Find (Artifact);

         if Artifact_Sets.Has_Element (C) then
            Self.Implicit_Inputs (Succ).Delete (C);
         end if;
      end loop;

      Self.Successors.Delete (C_Succ);
      C_Pred := Self.Predecessor.Find (Artifact);

      if Artifact_Action_Maps.Has_Element (C_Pred) then
         C := Self.Outputs
           (Artifact_Action_Maps.Element (C_Pred)).Find (Artifact);

         if Artifact_Sets.Has_Element (C) then
            Self.Outputs (Artifact_Action_Maps.Element (C_Pred)).Delete (C);
         end if;

         Self.Predecessor.Delete (C_Pred);
      end if;
   end Remove_Artifact;

   --------------
   -- Reporter --
   --------------

   function Reporter
     (Self : Object) return GPR2.Reporter.Holders.Reference_Type
   is
   begin
      return Self.Tree.Reporter;
   end Reporter;

   --------------------------
   -- Set_External_Options --
   --------------------------

   procedure Set_External_Options
     (Self    : in out Object;
      Options : GPR2.Build.External_Options.Object) is
   begin
      Self.External_Options := Options;
   end Set_External_Options;

   ------------
   -- Unload --
   ------------

   procedure Unload
     (Self : in out Object;
      Complete : Boolean := True) is
   begin
      Self.Build_Dbs.Clear;

      if Complete then
         Self.Self := null;
         Self.Tree := null;
         Self.External_Options.Clear;
      else
         Self.Create_View_Dbs;
      end if;
   end Unload;

begin

   GPR2.Tree_Internal.Init_Tree_Database := Create'Access;

end GPR2.Build.Tree_Db;
