--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Artifacts.Library;
with GPR2.Build.Tree_Db;
with Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Text_IO;

package body GPR2.Build.Actions.Ada_Bind is

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self : in out Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict)
   is
      pragma Unreferenced (Env);
   begin
      Args.Append ("gnatbind");
      Args.Append  (Self.Main_Ali.Path.String_Value);

      for C of Self.View.Closure loop
         if C.Is_Runtime then
            --  Lookup of runtime lib is done automatically by gnatbind
            null;
         elsif C.Is_Library then
            Args.Append ("-I" & C.Library_Ali_Directory.String_Value);
         elsif C.Kind in With_Object_Dir_Kind then
            Args.Append ("-I" & C.Object_Directory.String_Value);
         end if;
      end loop;

      Args.Append ("-o");

      --  Directories separator are not allowed. We must be in the correct
      --  directory and only use the source base name with extension.

      Args.Append (String (Self.Output_Body.Path.Simple_Name));
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature (Self : in out Object) is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      Self.Signature.Clear;

      for Pred of Self.Tree.Inputs (UID) loop
         if Pred in Artifacts.Library.Object'Class then
            Self.Signature.Add_Artifact (Pred);
         end if;
      end loop;

      for D of Self.Obj_Deps loop
         Self.Signature.Add_Artifact
           (Artifacts.Files.Create (Path_Name.Create_File (D)));
      end loop;

      Self.Signature.Add_Artifact (Self.Generated_Spec);
      Self.Signature.Add_Artifact (Self.Generated_Body);

      Self.Signature.Store (Self.Tree.Db_Filename_Path (UID));
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Object;
      Main_Ali : Artifacts.Files.Object;
      Context  : GPR2.Project.View.Object)
   is
   begin
      Self.Ctxt := Context;
      Self.Main_Ali := Main_Ali;
      Self.Output_Spec :=
        Artifacts.Files.Create
          (Context.Object_Directory.Compose (Self.BN & ".ads"));
      Self.Output_Body :=
        Artifacts.Files.Create
          (Context.Object_Directory.Compose (Self.BN & ".adb"));
      Self.Traces := Create ("ACTION_ADA_BIND");
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding procedure On_Tree_Insertion
     (Self     : in out Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object)
   is
      UID       : constant Actions.Action_Id'Class := Object'Class (Self).UID;
      Post_Bind : Actions.Post_Bind.Object;

   begin
      Db.Add_Output
        (UID,
         Self.Output_Spec,
         Messages);
      Db.Add_Output
        (UID,
         Self.Output_Body,
         Messages);

      Db.Add_Input (UID, Self.Main_Ali, True);

      Post_Bind :=
        Actions.Post_Bind.Create (Self.Output_Body, Self.View, Self);
      Db.Add_Action (Post_Bind, Messages);
      Db.Add_Input (Post_Bind.UID, Self.Output_Body, True);
   end On_Tree_Insertion;

   ---------------
   -- Post_Bind --
   ---------------

   function Post_Bind (Self : Object) return Actions.Post_Bind.Object is
   begin
      for Succ of Self.Tree.Successors (Self.Output_Body) loop
         return Actions.Post_Bind.Object (Succ);
      end loop;

      return Actions.Post_Bind.Undefined;
   end Post_Bind;

   ------------------
   -- Post_Command --
   ------------------

   overriding procedure Post_Command (Self : in out Object)
   is
      use Ada.Text_IO;
      use Ada.Strings;
      use Ada.Strings.Fixed;

      procedure Process_Option_Or_Object_Line (Line : String);
      --  Pass options to the linker. Do not pass object file lines,
      --  as the objects to link are already obtained by parsing ALI files.

      -----------------------------------
      -- Process_Option_Or_Object_Line --
      -----------------------------------

      procedure Process_Option_Or_Object_Line (Line : String) is
         Switch_Index : Natural := Index (Line, "--");
      begin
         if Switch_Index = 0 then
            raise Program_Error
              with "Failed parsing line " & Line & " from " &
              Self.Output_Body.Path.String_Value;
         end if;

         --  Skip the "--" comment prefix

         Switch_Index := Switch_Index + 2;

         declare
            Trimed_Line : constant String :=
                            Trim (Line (Switch_Index .. Line'Last), Both);
         begin
            if Trimed_Line (Trimed_Line'First) = '-' then
               Self.Linker_Opts.Append (Trimed_Line);
            else
               Self.Obj_Deps.Include (Filename_Type (Trimed_Line));
            end if;
         end;
      end Process_Option_Or_Object_Line;

      Src_File     : File_Type;
      Reading      : Boolean         := False;
      Begin_Marker : constant String := "--  BEGIN Object file/option list";
      End_Marker   : constant String := "--  END Object file/option list";

   begin
      Self.Traces.Trace
        ("Parsing file '" & Self.Output_Body.Path.String_Value &
           "' generated by " & Self.UID.Image &
           " to obtain linker options");

      Open
        (File => Src_File,
         Mode => In_File,
         Name => Self.Output_Body.Path.String_Value);

      while not End_Of_File (Src_File) loop
         declare
            Line : constant String := Get_Line (Src_File);
         begin
            if Index (Line, Begin_Marker) = Line'First then
               Reading := True;
            elsif Index (Line, End_Marker) = Line'First then
               Reading := False;
               exit;
            elsif Reading then
               Process_Option_Or_Object_Line (Line);
            end if;
         end;
      end loop;

      Close (Src_File);
   end Post_Command;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      BN     : constant Simple_Name := Self.Main_Ali.Path.Simple_Name;
      Result : constant Ada_Bind_Id :=
                 (Name_Len  => BN'Length,
                  Ali_Name  => BN,
                  Ctxt      => Self.Ctxt);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Ada_Bind;
