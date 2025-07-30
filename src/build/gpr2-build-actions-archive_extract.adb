--
--  Copyright (C) 2025 AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;

package body GPR2.Build.Actions.Archive_Extract is

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is
      pragma Unreferenced (Slot);
      package PRA renames GPR2.Project.Registry.Attribute;

      Driver : constant Project.Attribute.Object :=
                 Self.Ctxt.Attribute (PRA.Archive_Builder);

   begin
      if not Driver.Is_Defined then
         raise Action_Error with "no archive builder in this configuration";
      end if;

      Cmd_Line.Set_Driver (Driver.Values.First_Element.Text);
      Cmd_Line.Add_Argument ("-x");
      Cmd_Line.Add_Argument (Self.Archive.Path.String_Value);
      Cmd_Line.Add_Argument (String (Self.Extracted_Object.Path.Simple_Name));
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature (Self : in out Object; Load_Mode : Boolean) is
   begin
      if not Self.Signature.Add_Input (Self.Archive) and then Load_Mode then
         return;
      end if;

      if not Self.Signature.Add_Output (Self.Extracted_Object)
        and then Load_Mode
      then
         return;
      end if;
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self             : in out Object;
      Archive          : GPR2.Build.Artifacts.Library.Object;
      Extracted_Object : Simple_Name;
      View             : GPR2.Project.View.Object) is
   begin
      --  Ensure the object wasn't previously initialized prior to this call
      Self := Undefined;

      Self.Ctxt := View;
      Self.Archive := Archive;
      Self.Extracted_Object :=
        GPR2.Build.Artifacts.Object_File.Create
          (GPR2.Path_Name.Create_File
             (Extracted_Object, Self.Working_Directory.Value));
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      Db.Add_Input (UID, Self.Archive, True);

      if not Db.Add_Output (UID, Self.Extracted_Object) then
         return False;
      end if;

      return True;
   end On_Tree_Insertion;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      Archive_BN : constant Simple_Name := Self.Archive.Path.Simple_Name;
      Object_BN  : constant Simple_Name :=
        Self.Extracted_Object.Path.Simple_Name;
      Result     : constant Archive_Extract_Id :=
        (Archive_Name_Len     => Archive_BN'Length,
         Extr_Object_Name_Len => Object_BN'Length,
         View                 => Self.Ctxt,
         Archive              => Archive_BN,
         Object_To_Extract    => Object_BN);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Archive_Extract;
