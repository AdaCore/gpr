--
--  Copyright (C) 2024, AdaCore
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
     (Self : in out Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict;
      Slot : Positive)
   is
      pragma Unreferenced (Env, Slot);
   begin
      --  ??? Replace hard coded values

      Args.Append ("gcc");
      Args.Append ("-c");
      Args.Append (String (Self.Input.Path.Simple_Name));
      Args.Append ("-o");
      Args.Append (String (Self.Output.Path.Simple_Name));
   end Compute_Command;

   ------------
   -- Create --
   ------------

   function Create
     (Impl   : Artifacts.Files.Object;
      View   : GPR2.Project.View.Object;
      Binder : GPR2.Build.Actions.Ada_Bind.Object) return Object
   is
      Attr : constant GPR2.Project.Attribute.Object :=
               View.Attribute (PRA.Compiler.Object_File_Suffix,
                               PAI.Create (Ada_Language));
      O_Suff : constant Simple_Name :=
                 (if Attr.Is_Defined then Simple_Name (Attr.Value.Text)
                  else ".o");
      Self   : Object;

   begin
      Self.Traces := Create ("ACTION_POST_BIND");
      Self.View   := View;
      Self.Binder := Binder;
      Self.Input  := Impl;
      Self.Output :=
        Artifacts.Files.Create
          (View.Object_Directory.Compose (Impl.Path.Base_Filename & O_Suff));

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
            for Opt of Binder_Action.Linker_Options loop
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
