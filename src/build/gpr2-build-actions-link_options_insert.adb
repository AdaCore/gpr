--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.FS;
with GNATCOLL.Traces;
with GPR2.Message;
with GPR2.Build.Actions.Link;

package body GPR2.Build.Actions.Link_Options_Insert is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPR.BUILD.ACTIONS.LINK_OPTIONS_INSERT", GNATCOLL.Traces.Off);

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option (Self : in out Object; Option : String) is
   begin
      Self.Options.Append (Option);
   end Add_Option;

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
      GPR_Section  : constant String := ".GPR.linker_options";
      Options_File : Path_Name.Object;
   begin
      if not Signature_Only then
         declare
            File                      : constant Tree_Db.Temp_File :=
              Self.Get_Or_Create_Temp_File ("linker_options", Local);
            Number_Of_Options_Written : Integer := 0;
            Number_Of_Options         : constant Integer :=
              Integer (Self.Options.Length);

            use all type GNATCOLL.OS.FS.File_Descriptor;
         begin
            if File.FD = GNATCOLL.OS.FS.Null_FD then
               Self.Tree.Reporter.Report
                 (GPR2.Message.Create
                    (GPR2.Message.Error,
                     "failed to initialize a temporary file "
                     & "to contain the linker options"));

               raise Action_Error;
            end if;

            Options_File := GPR2.Path_Name.Create_File (File.Path);

            for Option of Self.Options loop
               Write (File.FD, Option);
               Number_Of_Options_Written := Number_Of_Options_Written + 1;

               --  Write a new line after each option, except the last one

               if Number_Of_Options_Written < Number_Of_Options then
                  Write (File.FD, "" & ASCII.LF);
               end if;
            end loop;

            Close (File.FD);
         end;
      end if;

      Cmd_Line.Set_Driver
        (Self.View.Compiler_Prefix (Ada_Language) & "objcopy");
      Cmd_Line.Add_Argument ("-j");
      Cmd_Line.Add_Argument (GPR_Section);
      Cmd_Line.Add_Argument ("--add-section");

      if not Signature_Only then
         Cmd_Line.Add_Argument
           (GPR_Section & "=" & String (Options_File.Simple_Name),
            GPR2.Build.Command_Line.Ignore);
      end if;

      Cmd_Line.Add_Argument (Self.Input_Object_File.Path);
      Cmd_Line.Add_Argument (Self.Output_Object_File.Path);
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature (Self : in out Object; Load_Mode : Boolean) is
   begin
      if not Self.Signature.Add_Input (Self.Input_Object_File)
        and then Load_Mode
      then
         return;
      end if;

      for Opt of Self.Options loop
         if not Self.Signature.Add_Input
           (Artifacts.Key_Value.Create ("linker-option", Opt))
           and then Load_Mode
         then
            return;
         end if;
      end loop;

      if not Self.Signature.Add_Output (Self.Output_Object_File)
        and then Load_Mode
      then
         return;
      end if;
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : in out Object;
      Object_File : Artifacts.Object_File.Object;
      Options     : Containers.Value_List := Containers.Empty_Value_List;
      View        : GPR2.Project.View.Object)
   is
   begin
      Self.Input_Object_File := Object_File;
      Self.Output_Object_File :=
        Artifacts.Object_File.Create
          (View.Object_Directory.Compose
             ("o__" & View.Library_Name & Object_File.Path.Extension));
      Self.Options := Options;
      Self.Ctxt := View;
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
      Db.Add_Input (UID, Self.Input_Object_File, True);

      if not Db.Add_Output (UID, Self.Output_Object_File) then
         return False;
      end if;

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
      Successors : Tree_Db.Actions_List'Class :=
        Self.Tree.Successors (Self.Output_Object_File);
   begin
      for Act of Successors loop
         if Act in Link.Object'Class then
            for Opt of Self.Options loop
               Traces.Trace
                 ("Adding option """ & Opt & """ to " & Act.UID.Image);
               Link.Object'Class (Act).Add_Option (Opt);
            end loop;
         end if;
      end loop;

      return True;
   end Post_Command;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      BN     : constant Simple_Name :=
                 Self.Output_Object_File.Path.Simple_Name;
      Result : constant Link_Options_Insert_Id :=
        (Name_Len => BN'Length, View => Self.Ctxt, Object_File => BN);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Link_Options_Insert;
