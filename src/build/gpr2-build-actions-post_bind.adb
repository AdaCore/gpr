--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.ALI_Parser;
with GPR2.Build.External_Options;
with GPR2.Build.Tree_Db;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.View;

package body GPR2.Build.Actions.Post_Bind is

   package PAI renames GPR2.Project.Attribute_Index;

   -------------
   -- Command --
   -------------

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
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

      --  Add -cargs and -cargs:<lang>
      for Arg of Self.Tree.External_Options.Fetch
                   (External_Options.Compiler, Ada_Language)
      loop
         if Arg /= "-gnatg"
           and then not GNATCOLL.Utils.Starts_With (Arg, "-gnaty")
         then
            Cmd_Line.Add_Argument (Arg);
         end if;
      end loop;

      --  Get the command line used for the first ALI given as input and
      --  add some of its switches

      declare
         Ali : GPR2.Build.Artifacts.Files.Object;
      begin
         for Input of Self.Tree.Inputs (Self.Binder.UID) loop
            Ali := Artifacts.Files.Object (Input);
            exit;
         end loop;

         if Ali.Is_Defined and then Ali.Path.Exists then
            for Sw of ALI_Parser.Switches (Ali.Path) loop
               if not GNATCOLL.Utils.Starts_With (Sw, "-I")
                 and then not GNATCOLL.Utils.Starts_With (Sw, "-gnat")
                 and then not GNATCOLL.Utils.Starts_With (Sw, "--RTS=")
               then
                  Cmd_Line.Add_Argument (Sw);
               end if;
            end loop;
         end if;
      end;

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

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      if not Self.Signature.Add_Input (Self.Input, Check_Checksums)
      then
         return;
      end if;

      if not Self.Signature.Add_Input (Self.Ali, Check_Checksums) then
         return;
      end if;

      if not Self.Signature.Add_Output (Self.Output, Check_Checksums) then
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
      Self.Ctxt   := View;
      Self.Binder := Binder;
      Self.Input  := Impl;
      Self.Output :=
        Artifacts.Object_File.Create
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

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Action_Id'Class is
      BN : constant Filename_Type := Self.Input.Path.Simple_Name;
   begin
      return Post_Bind_Id'(BN'Length, BN, Self.View);
   end UID;

end GPR2.Build.Actions.Post_Bind;
