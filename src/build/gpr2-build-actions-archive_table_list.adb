--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Fixed;
with GPR2.Build.Actions.Archive_Extract;
with GNAT.String_Split;
with GPR2.Build.Actions.Link;
with GPR2.Build.Actions.Link_Options_Extract;

package body GPR2.Build.Actions.Archive_Table_List is

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is
      pragma Unreferenced (Slot);
   begin
      Cmd_Line.Set_Driver ("ar");
      Cmd_Line.Add_Argument ("-t");
      Cmd_Line.Add_Argument (Self.Archive.Path.String_Value);
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
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Object;
      Archive : GPR2.Build.Artifacts.Library.Object;
      View    : GPR2.Project.View.Object) is
   begin
      Self.Ctxt := View;
      Self.Archive := Archive;
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

      return True;
   end On_Tree_Insertion;

   ------------------
   -- Post_Command --
   ------------------

   overriding
   function Post_Command
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean
   is
      use GNAT.String_Split;
      Separators           : constant String := ASCII.LF & ASCII.CR & "";
      Sliced_Object_Files  : Slice_Set;
      Linkers_UID          : Action_Id_Sets.Set := Action_Id_Sets.Empty_Set;
      Archive_Extract      : GPR2.Build.Actions.Archive_Extract.Object;
      Link_Options_Extract : GPR2.Build.Actions.Link_Options_Extract.Object;
   begin

      for Act of Self.Tree.Successors (Self.UID_Artifact) loop
         if Act in GPR2.Build.Actions.Link.Object'Class then
            Linkers_UID.Insert (Act.UID);
         end if;
      end loop;

      Create
        (S          => Sliced_Object_Files,
         From       => To_String (Stdout),
         Separators => Separators,
         Mode       => Multiple);

      for I in 1 .. Slice_Count (Sliced_Object_Files) loop
         declare
            Object_To_Extract : constant String :=
              Ada.Strings.Fixed.Trim
                (Slice (Sliced_Object_Files, I), Ada.Strings.Both);

         begin
            if Object_To_Extract'Length > 3
              and then (Object_To_Extract
                         (Object_To_Extract'First
                          .. Object_To_Extract'First + 2)
                       = "b__" or else
                         Object_To_Extract
                         (Object_To_Extract'First
                          .. Object_To_Extract'First + 2) = "p__")
            then
               Archive_Extract.Initialize
                 (Self.Archive, Simple_Name (Object_To_Extract), Self.Ctxt);

               if not Self.Tree.Add_Action (Archive_Extract) then
                  return False;
               end if;

               Link_Options_Extract.Initialize
                 (Simple_Name (Object_To_Extract), Self.Ctxt);

               if not Self.Tree.Add_Action (Link_Options_Extract) then
                  return False;
               end if;

               Self.Tree.Add_Input
                 (Link_Options_Extract.UID,
                  Archive_Extract.Extracted_Object,
                  True);

               for Linker_UID of Linkers_UID loop
                  Self.Tree.Add_Input
                    (Linker_UID, Link_Options_Extract.UID_Artifact, True);
               end loop;

               return True;
            end if;
         end;
      end loop;
      return True;
   end Post_Command;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      BN     : constant Simple_Name := Self.Archive.Path.Simple_Name;
      Result : constant Archive_Table_List_Id :=
        (Name_Len => BN'Length, View => Self.Ctxt, Archive => BN);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Archive_Table_List;
