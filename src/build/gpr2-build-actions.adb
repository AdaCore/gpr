--
--  Copyright (C) 2023-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;

with GNATCOLL.OS.FS;
with GNATCOLL.OS.FSUtil;
with GNATCOLL.Traces;

with GPR2.Build.Tree_Db;

package body GPR2.Build.Actions is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create
                ("GPR.BUILD.ACTIONS",
                 GNATCOLL.Traces.Off);

   Command_Line_Key : constant String := "command_line";

   ------------
   -- Attach --
   ------------

   procedure Attach (Self : in out Object'Class;
                     Db   : in out GPR2.Build.Tree_Db.Object)
   is
   begin
      Self.Tree := Db.Ref;
   end Attach;

   ------------------------
   -- Cleanup_Temp_Files --
   ------------------------

   procedure Cleanup_Temp_Files
     (Self : in out Object'Class;
      Scope : Temp_File_Scope)
   is
      use GNATCOLL.OS.FSUtil;
   begin
      if Scope = Global then
         Self.Tree.Clear_Temp_Files;
      else
         for F of Self.Tmp_Files loop
            if not Remove_File
              (Self.View.Object_Directory.Compose (F).String_Value)
            then
               Traces.Trace
                 ("error: could not remove temp file " & String (F) & " in " &
                    Self.View.Object_Directory.String_Value);
            end if;
         end loop;
      end if;

      Self.Tmp_Files.Clear;
   end Cleanup_Temp_Files;

   ----------------------------
   -- Compute_Response_Files --
   ----------------------------

   procedure Compute_Response_Files
     (Self           : in out Object;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean) is
   begin
      null;
   end Compute_Response_Files;

   -----------------
   -- Db_Filename --
   -----------------

   function Db_Filename
     (Self     : Action_Id'Class;
      Basename : Boolean := False) return Simple_Name
   is
      use Ada.Characters.Handling;
      use Ada.Strings;

      Space_Repl : constant Ada.Strings.Maps.Character_Mapping :=
                     Maps.To_Mapping (" ", "_");
      Res        : Unbounded_String;

   begin
      --  A Build_Db file item is stored in the view's object directory so
      --  there's no need to add the View identifier in it.
      Append (Res, '.');

      if Self.Language /= No_Language then
         Append (Res, To_Lower (Image (Self.Language)));
         Append (Res, '_');
      end if;

      Append (Res, Fixed.Translate (To_Lower (Self.Action_Class), Space_Repl));
      Append (Res, '_');

      Append (Res, Self.Action_Parameter);

      if not Basename then
         Append (Res, ".json");
      end if;

      return Simple_Name (To_String (Res));
   end Db_Filename;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate (Self : in out Object) is
   begin
      Self.Deactivated := True;
   end Deactivate;

   -----------------------------
   -- Get_Or_Create_Temp_File --
   -----------------------------

   function Get_Or_Create_Temp_File
     (Self      : in out Object'Class;
      Purpose   : Filename_Type;
      Scope     : Temp_File_Scope;
      Extension : Simple_Name := ".tmp") return Tree_Db.Temp_File
   is
   begin
      if Scope = Global then
         return Self.Tree.Get_Or_Create_Temp_File
           (Self.View, Purpose, Extension);
      else
         declare
            --  ??? Naive implementation as first try
            BN   : constant Filename_Type :=
                     Self.UID.Db_Filename (True) & "-" & Purpose & Extension;
            Dest : constant GPR2.Path_Name.Object :=
                     Self.View.Object_Directory.Compose (BN);
            FD   : GNATCOLL.OS.FS.File_Descriptor;
            use GNATCOLL.OS.FS;

         begin
            if Self.Tmp_Files.Contains (BN) then
               FD := Null_FD;
            else
               FD := GNATCOLL.OS.FS.Open
                 (Dest.String_Value,
                  GNATCOLL.OS.FS.Write_Mode);
               pragma Assert (FD /= Null_FD and then FD /= Invalid_FD,
                              "could not create " & Dest.String_Value);
               Self.Tmp_Files.Insert (BN);
            end if;

            return (Path_Len => Dest.Value'Length,
                    FD       => FD,
                    Path     => Dest.Value);
         end;
      end if;
   end Get_Or_Create_Temp_File;

   -----------
   -- Image --
   -----------

   function Image
     (Self      : Action_Id'Class;
      With_View : Boolean := True) return String
   is
      Res : Unbounded_String;
   begin
      Append (Res, "[");
      if Self.Language /= No_Language then
         Append (Res, Image (Self.Language));
         Append (Res, ' ');
      end if;

      Append (Res, Self.Action_Class);
      Append (Res, "] ");
      Append (Res, Self.Action_Parameter);

      if With_View then
         Append (Res, " (");
         Append (Res, String (Self.View.Path_Name.Simple_Name));
         Append (Res, ")");
      end if;

      return -Res;
   end Image;

   --------------------
   -- Load_Signature --
   --------------------

   procedure Load_Signature (Self : in out Object'Class)
   is
      Db_File  : constant GPR2.Path_Name.Object :=
                     Self.Tree.Db_Filename_Path (Self.UID, True);
      Cmd_Line : GPR2.Build.Command_Line.Object;
      Ign      : Boolean with Unreferenced;

   begin
      if not Db_File.Is_Defined then
         Self.Signature.Clear;

         return;
      end if;

      Self.Signature := Build.Signature.Load (Db_File, Self.View);

      Self.Compute_Signature (Load_Mode => True);

      if Self.Signature.Was_Saved then
         --  The signature hasn't been invalidated for now, so the last
         --  element to check is its command line
         Cmd_Line :=
           GPR2.Build.Command_Line.Create (Self.Working_Directory);
         Self.Compute_Command (1, Cmd_Line, Signature_Only => True);
         Self.Compute_Response_Files (Cmd_Line, Signature_Only => True);

         Ign := Self.Signature.Add_Input
           (Artifacts.Key_Value.Create
              (Command_Line_Key, Cmd_Line.Signature));
      end if;

   exception
      when others =>
         Self.Signature.Clear;
   end Load_Signature;

   ---------------
   -- Serialize --
   ---------------

   function Serialize (Self : Action_Id'Class) return String is
         Res : Unbounded_String;
   begin
      Append (Res, GPR2.View_Ids.Image (Self.View.Id));

      if Self.Language /= No_Language then
         Append (Res, ":");
         Append (Res, Image (Self.Language));
         Append (Res, ":");
      end if;

      Append (Res, ":");
      Append (Res, Self.Action_Class);
      Append (Res, ":");
      Append (Res, Self.Action_Parameter);

      return -Res;
   end Serialize;

   -------------------------
   -- Update_Command_Line --
   -------------------------

   procedure Update_Command_Line
     (Self : in out Object'Class;
      Slot : Positive)
   is
      Ign : Boolean with Unreferenced;
   begin
      Self.Cmd_Line :=
        GPR2.Build.Command_Line.Create (Self.Working_Directory);
      Self.Compute_Command (Slot, Self.Cmd_Line, False);
      Self.Compute_Response_Files (Self.Cmd_Line, False);

      if Self.Cmd_Line.Total_Length = 0
        and then not Self.Deactivated
      then
         raise Action_Error;
      end if;
   end Update_Command_Line;

   ---------------------
   -- Write_Signature --
   ---------------------

   function Write_Signature
     (Self   : in out Object'Class;
      Stdout : Unbounded_String;
      Stderr : Unbounded_String) return Boolean
   is
      UID : constant Action_Id'Class := Self.UID;
      Ign : Boolean with Unreferenced;
   begin
      --  Ensure the inputs and outputs are up-to-date after the action is
      --  executed: the list of presumed inputs and outputs may need to be
      --  refined after the fact.

      Self.Signature.Clear;
      Self.Compute_Signature (Load_Mode => False);

      if Self.Signature.Is_Empty then
         return False;
      end if;

      Ign := Self.Signature.Add_Input
        (Artifacts.Key_Value.Create
           (Command_Line_Key, Self.Cmd_Line.Signature));

      Self.Signature.Add_Console_Output (Stdout, Stderr);

      Self.Signature.Store (Self.Tree.Db_Filename_Path (UID, False));

      return True;
   end Write_Signature;

end GPR2.Build.Actions;
