--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Link;
with GPR2.Build.Tree_Db;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Tree;
with GPR2.Project.View;

package body GPR2.Build.Actions.Post_Bind is

   package PAI renames GPR2.Project.Attribute_Index;

   -------------
   -- Command --
   -------------

   overriding procedure Compute_Command
     (Self     : in out Object;
      Slot     : Positive;
      Cmd_Line : in out GPR2.Build.Command_Line.Object)
   is
      pragma Unreferenced (Slot);

      procedure Add_Attr
        (Id      : Q_Attribute_Id;
         Idx     : PAI.Object;
         Is_List : Boolean);

      --------------
      -- Add_Attr --
      --------------

      procedure Add_Attr
        (Id      : Q_Attribute_Id;
         Idx     : PAI.Object;
         Is_List : Boolean)
      is
         Attr : constant GPR2.Project.Attribute.Object :=
                  Self.View.Attribute (Id, Idx);
      begin
         if not Attr.Is_Defined then
            return;
         end if;

         if Is_List then
            for Val of Attr.Values loop
               Cmd_Line.Add_Argument (Val.Text);
            end loop;
         else
            Cmd_Line.Add_Argument (Attr.Value.Text);
         end if;
      end Add_Attr;

      Ada_Lang    : constant PAI.Object := PAI.Create (Ada_Language);
      Driver_Attr : constant GPR2.Project.Attribute.Object :=
        Self.View.Attribute (PRA.Compiler.Driver, Ada_Lang);
   begin
      if Driver_Attr.Is_Defined then
         Cmd_Line.Set_Driver (Driver_Attr.Value.Text);
      else
         return;
      end if;

      Add_Attr (PRA.Compiler.Leading_Required_Switches, Ada_Lang, True);
      Cmd_Line.Add_Argument ("-c");
      Cmd_Line.Add_Argument ("-gnatA");
      Cmd_Line.Add_Argument ("-gnatiw");
      Cmd_Line.Add_Argument ("-gnatws");
      Cmd_Line.Add_Argument ("-gnatWb");
      Cmd_Line.Add_Argument (Self.Input.Path);
      Cmd_Line.Add_Argument ("-o");
      Cmd_Line.Add_Argument (Self.Output.Path);

      if Self.View.Is_Library and then Self.View.Library_Kind /= "static" then
         Add_Attr (PRA.Compiler.Pic_Option, Ada_Lang, True);
      end if;

      Add_Attr (PRA.Compiler.Trailing_Required_Switches, Ada_Lang, True);
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean)
   is
   begin
      if not Self.Signature.Add_Input (Self.Input) and then Load_Mode then
         return;
      end if;

      if not Self.Signature.Add_Input (Self.Ali) and then Load_Mode then
         return;
      end if;

      if not Self.Signature.Add_Output (Self.Output) and then Load_Mode then
         return;
      end if;
   end Compute_Signature;

   ------------
   -- Create --
   ------------

   function Create
     (Impl   : Artifacts.Files.Object;
      View   : GPR2.Project.View.Object;
      Binder : GPR2.Build.Actions.Ada_Bind.Object;
      Skip   : Boolean) return Object
   is
      Attr : constant GPR2.Project.Attribute.Object :=
               View.Attribute (PRA.Compiler.Object_File_Suffix,
                               PAI.Create (Ada_Language));
      O_Suff : constant Simple_Name :=
                 (if Attr.Is_Defined then Simple_Name (Attr.Value.Text)
                  else ".o");
      Self   : Object;

   begin
      Self.Traces := Create ("ACTION_POST_BIND",
                             GNATCOLL.Traces.Off);
      Self.View   := View;
      Self.Binder := Binder;
      Self.Input  := Impl;
      Self.Output :=
        Artifacts.Files.Create
          (View.Object_Directory.Compose (Impl.Path.Base_Filename & O_Suff));
      Self.Ali    :=
        Artifacts.Files.Create
          (View.Object_Directory.Compose (Impl.Path.Base_Filename & ".ali"));
      Self.Skip   := Skip;

      if Skip then
         Self.Deactivate;
      end if;

      return Self;
   end Create;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      if not Db.Add_Output (UID, Self.Output) then
         return False;
      end if;

      if not Db.Add_Output (UID, Self.Ali) then
         return False;
      end if;

      Db.Add_Input (UID, Self.Input, True);

      return True;
   end On_Tree_Insertion;

   ------------------
   -- Post_Command --
   ------------------

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status) return Boolean
   is
      Binder_Action   : constant Ada_Bind.Object :=
                          Ada_Bind.Object
                            (Self.View.Tree.Artifacts_Database.Action
                               (Self.Binder.UID));
      Successors      : Tree_Db.Actions_List'Class :=
                          Self.Tree.Successors (Self.Output);

   begin
      for Act of Successors loop
         if Act in Link.Object'Class then
            Self.Traces.Trace ("Options passed to " & Act.UID.Image & ":");
            for Opt of Binder_Action.Linker_Options loop
               Self.Traces.Trace ("* '" & Opt & "'");
               Link.Object (Act).Add_Option (Opt);
            end loop;
         end if;
      end loop;

      return True;
   end Post_Command;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Action_Id'Class is
      BN : constant Filename_Type := Self.Input.Path.Simple_Name;
   begin
      return Post_Bind_Id'(BN'Length, BN, Self.View);
   end UID;

end GPR2.Build.Actions.Post_Bind;
