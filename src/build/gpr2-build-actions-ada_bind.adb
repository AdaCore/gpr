--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--


with Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Text_IO;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with GNATCOLL.Utils; use GNATCOLL.Utils;
with GNATCOLL.OS.FS;

with GPR2.Build.Actions.Post_Bind;
pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Build.Tree_Db;
with GPR2.External_Options;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;

package body GPR2.Build.Actions.Ada_Bind is

   use GNAT;

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;


   procedure Initialize_Linker_Options (Self : in out Object);
   --  Adjust the linker options in case of shared or static cases

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self : in out Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict;
      Slot : Positive)
   is
      pragma Unreferenced (Env);

      procedure Add_Arg (Arg : String);

      function Add_Attr
        (Id      : Q_Attribute_Id;
         Index   : PAI.Object;
         Is_List : Boolean) return Boolean;

      function Add_Attr_RS
        (Id    : Q_Attribute_Id;
         Index : PAI.Object) return Boolean;

      procedure Add_Binder (Id : Q_Attribute_Id; Index : PAI.Object);

      procedure Add_Mapping_File;

      procedure Create_Response_File;

      procedure Enforce_DashA_Absolute_Path;

      Lang_Ada_Idx : constant PAI.Object := PAI.Create (GPR2.Ada_Language);
      Status       : Boolean;
      CL_Size      : Integer := 0;

      -------------
      -- Add_Arg --
      -------------

      procedure Add_Arg (Arg : String) is
      begin
         CL_Size := CL_Size + Arg'Length;

         if not Args.Is_Empty then
            --  Take into account the need of " " between args
            CL_Size := CL_Size + 1;
         end if;

         Args.Append (Arg);
      end Add_Arg;

      --------------
      -- Add_Attr --
      --------------

      function Add_Attr
        (Id      : Q_Attribute_Id;
         Index   : PAI.Object;
         Is_List : Boolean) return Boolean
      is
         Attr : constant Project.Attribute.Object :=
                  Self.View.Attribute (Id, Index);
      begin
         if not Attr.Is_Defined then
            return False;
         end if;

         if Is_List then
            for Idx in Attr.Values.First_Index .. Attr.Values.Last_Index loop
               Add_Arg (Attr.Values.Element (Idx).Text);
            end loop;
         else
            Add_Arg (Attr.Value.Text);
         end if;

         return True;
      end Add_Attr;

      -----------------
      -- Add_Attr_RS --
      -----------------

      function Add_Attr_RS
        (Id    : Q_Attribute_Id;
         Index : PAI.Object) return Boolean
      is
         Attr : constant Project.Attribute.Object :=
                  Self.View.Attribute (Id, Index);

         Gnatbind_Prefix_Equal : constant String := "gnatbind_prefix=";
         Gnatbind_Path_Equal   : constant String := "--gnatbind_path=";
         Ada_Binder_Equal      : constant String := "ada_binder=";
      begin
         if not Attr.Is_Defined then
            return False;
         end if;

         for Idx in Attr.Values.First_Index .. Attr.Values.Last_Index loop
            declare
               Str : constant String := Attr.Values.Element (Idx).Text;
            begin
               if not Starts_With (Str, Gnatbind_Path_Equal)
                 and then not Starts_With (Str, Gnatbind_Prefix_Equal)
                 and then not Starts_With (Str, Ada_Binder_Equal)
                 --  Ignore -C, as the generated sources are always in Ada
                 and then Str /= "-C"
               then
                  Add_Arg (Str);
               end if;
            end;
         end loop;

         return True;
      end Add_Attr_RS;

      ----------------
      -- Add_Binder --
      ----------------

      procedure Add_Binder (Id : Q_Attribute_Id; Index : PAI.Object) is
         Attr : constant Project.Attribute.Object :=
                  Self.View.Attribute (Id, Index);

         Gnatbind_Prefix_Equal       : constant String := "gnatbind_prefix=";
         Gnatbind_Path_Equal         : constant String := "--gnatbind_path=";
         Ada_Binder_Equal            : constant String := "ada_binder=";
         Default_Binder_Name         : constant String := "gnatbind";
         Tmp_Binder                  : GNAT.OS_Lib.String_Access;
         Tmp_Binder_Path             : GNAT.OS_Lib.String_Access;
         True_Binder_Path            : GNAT.OS_Lib.String_Access;
         Normalized_True_Binder_Path : GNAT.OS_Lib.String_Access;

         use type GNAT.OS_Lib.String_Access;
      begin
         if Attr.Is_Defined then
            --  Retrieve informations about the binder by looking for
            --  "gnatbind_prefix=", "--gnatbind_path=" or "ada_binder=".

            for Idx in Attr.Values.First_Index .. Attr.Values.Last_Index loop
               declare
                  Str : constant String := Attr.Values.Element (Idx).Text;
               begin
                  if Starts_With (Str, Gnatbind_Path_Equal)
                    and then Str'Length > Gnatbind_Path_Equal'Length
                  then
                     Tmp_Binder := new String'
                       (Str (Str'First +
                            Gnatbind_Path_Equal'Length .. Str'Last));
                     exit;

                  elsif Starts_With (Str, Gnatbind_Prefix_Equal)
                    and then Str'Length > Gnatbind_Prefix_Equal'Length
                  then
                     --  There is always a '-' between <prefix> and
                     --  "gnatbind". Add one if not already in <prefix>.
                     Tmp_Binder := new String'
                       (Str (Str'First +
                            Gnatbind_Prefix_Equal'Length .. Str'Last)
                        & (if Str (Str'Last) = '-' then "" else "-")
                        & Default_Binder_Name);
                     exit;

                  elsif Starts_With (Str, Ada_Binder_Equal)
                    and then Str'Length > Ada_Binder_Equal'Length
                  then
                     Tmp_Binder := new String'
                       (Str (Str'First +
                            Ada_Binder_Equal'Length .. Str'Last));
                     exit;
                  end if;
               end;
            end loop;
         end if;

         --  if "gnatbind_prefix=", "--gnatbind_path=" or "ada_binder=" weren't
         --  found, default to "gnatbind".

         if Tmp_Binder = null then
            Tmp_Binder := new String'(Default_Binder_Name);
         end if;

         --  if we don't have an absolute path to gnatbind at this point, try
         --  to find it in the same install as the compiler.

         declare
            Compiler_Driver : constant Project.Attribute.Object :=
                                Self.View.Attribute
                                  (PRA.Compiler.Driver, Lang_Ada_Idx);
         begin
            if not OS_Lib.Is_Absolute_Path (Tmp_Binder.all)
              and then Compiler_Driver.Is_Defined
            then
               Tmp_Binder_Path := new String'
                 (Directory_Operations.Dir_Name (Compiler_Driver.Value.Text)
                  & OS_Lib.Directory_Separator & Tmp_Binder.all);
            end if;
         end;

         --  At this point we try to locate either the absolute path to
         --  gnatbind or the gnatbind in the same install as the compiler.

         True_Binder_Path := new String'
           (Locate_Exec_On_Path ((
            if Tmp_Binder_Path /= null
            then Tmp_Binder_Path.all
            else Tmp_Binder.all)));

         --  We couldn't locate the absolute path to gnatbind nor the gnatbind
         --  in the same install as the compiler, try to find a gnatbind in the
         --  path.

         if True_Binder_Path.all = ""
           and then Tmp_Binder_Path /= null
         then
            OS_Lib.Free (True_Binder_Path);
            True_Binder_Path := new String'
              (Locate_Exec_On_Path (Tmp_Binder.all));
         end if;

         if Tmp_Binder_Path /= null then
            GNAT.OS_Lib.Free (Tmp_Binder_Path);
         end if;

         --  Try to Normalize the path, so that gnaampbind does not complain
         --  about not being in a "bin" directory. But don't resolve symbolic
         --  links, because in GNAT 5.01a1 and previous releases, gnatbind was
         --  a symbolic link for .gnat_wrapper.
         --  If the specified tool does not exist, return the its raw value.
         Normalized_True_Binder_Path := new String'
           (if True_Binder_Path.all /= ""
            then OS_Lib.Normalize_Pathname
              (True_Binder_Path.all, Resolve_Links => False)
            else Tmp_Binder.all);

         Add_Arg (Normalized_True_Binder_Path.all);

         GNAT.OS_Lib.Free (Tmp_Binder);
         GNAT.OS_Lib.Free (True_Binder_Path);
         GNAT.OS_Lib.Free (Normalized_True_Binder_Path);
      end Add_Binder;

      ----------------------
      -- Add_Mapping_File --
      ----------------------

      procedure Add_Mapping_File
      is
         use GNATCOLL.OS.FS;
         Map_File     : constant Tree_Db.Temp_File :=
                          Self.Get_Or_Create_Temp_File
                            ("bind_mapping", Global);
         S_Suffix     : constant String :=
                          Self.View.Attribute
                            (PRA.Compiler.Mapping_Spec_Suffix,
                             PAI.Create (Ada_Language)).Value.Text;
         B_Suffix     : constant String :=
                          Self.View.Attribute
                            (PRA.Compiler.Mapping_Body_Suffix,
                             PAI.Create (Ada_Language)).Value.Text;
         use Standard.Ada.Characters.Handling;

      begin
         if Map_File.FD /= Invalid_FD and then Map_File.FD /= Null_FD then
            for Src of Self.View.Visible_Sources loop
               if not Src.Owning_View.Is_Runtime
                 and then Src.Language = Ada_Language
               then
                  for U of Src.Units loop
                     if U.Kind /= S_No_Body then
                        declare
                           Key : constant String :=
                                   To_Lower (String (U.Full_Name)) &
                                   (if U.Kind = S_Spec
                                    then S_Suffix else B_Suffix);
                        begin
                           Write
                             (Map_File.FD,
                              Key & ASCII.LF &
                                String (Src.Path_Name.Simple_Name) & ASCII.LF &
                                Src.Path_Name.String_Value & ASCII.LF);
                        end;
                     end if;
                  end loop;
               end if;
            end loop;

            GNATCOLL.OS.FS.Close (Map_File.FD);
         end if;

         Add_Arg ("-F=" & String (Map_File.Path));
      end Add_Mapping_File;

      --------------------------
      -- Create_Response_File --
      --------------------------

      procedure Create_Response_File
      is
         use GNATCOLL.OS.FS;
         Resp_File : constant Tree_Db.Temp_File :=
                       Self.Get_Or_Create_Temp_File ("response_file", Local);
         New_Args  : GNATCOLL.OS.Process.Argument_List;
      begin
         New_Args.Append (Args.First_Element);

         if Resp_File.FD /= Null_FD then
            for Arg of Args loop
               if not (Arg = Args.First_Element) then
                  declare
                     Char          : Character;
                     Quotes_Needed : Boolean := False;
                  begin
                     for Index in Arg'Range loop
                        Char := Arg (Index);

                        if Char = ' '
                          or else Char = ASCII.HT
                          or else Char = '"'
                        then
                           Quotes_Needed := True;
                           exit;
                        end if;
                     end loop;

                     if Quotes_Needed then
                        declare
                           New_Arg : String (1 .. Arg'Length * 2 + 2);
                           Offset  : Integer := 0;
                        begin
                           New_Arg (1) := '"';
                           Offset := Offset + 1;

                           for Index in Arg'Range loop
                              Char := Arg (Index);
                              New_Arg (Index + Offset) := Char;

                              if Char = '"' then
                                 Offset := Offset + 1;
                                 New_Arg (Index + Offset) := '"';
                              end if;
                           end loop;

                           Offset := Offset + 1;
                           New_Arg (Arg'Length + Offset) := '"';

                           Write
                             (Resp_File.FD,
                              New_Arg (1 .. Arg'Length + Offset) & ASCII.LF);
                        end;
                     else
                        Write (Resp_File.FD, Arg & ASCII.LF);
                     end if;
                  end;
               end if;
            end loop;
         end if;

         New_Args.Append ("@" & String (Resp_File.Path));

         Args := New_Args;
      end Create_Response_File;

      ---------------------------------
      -- Enforce_DashA_Absolute_Path --
      ---------------------------------

      procedure Enforce_DashA_Absolute_Path is
         DashA        : constant String := "-A=";
         Prj_Root_Dir : constant Path_Name.Object :=
                          Self.Ctxt.Tree.Root_Project.Dir_Name;
      begin
         for Cursor in Args.Iterate loop
            declare
               Arg : constant String := Args (Cursor);
            begin
               if Starts_With (Arg, DashA) and then Arg'Length > DashA'Length
               then
                  declare
                     File : constant String :=
                              Arg (Arg'First + DashA'Length .. Arg'Last);
                  begin
                     if not OS_Lib.Is_Absolute_Path (File) then
                        declare
                           NF : constant String :=
                                  OS_Lib.Normalize_Pathname
                                    (File, String (Prj_Root_Dir.Name),
                                     Resolve_Links => False);
                        begin
                           CL_Size := CL_Size - Arg'Length;
                           CL_Size := CL_Size + DashA'Length + NF'Length;
                           Args.Replace_Element (Cursor, DashA & NF);
                        end;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end Enforce_DashA_Absolute_Path;

   begin
      --  [eng/gpr/gpr-issues#446] We should rework how the binder tools is
      --  fetched from the KB.
      --  Right now, gnatbind is called no matter what and
      --  Binder.Required_Switches is parsed to fetch gnatbind prefix or path.
      --  The binder tool should simply be stored in Binder.Driver
      --  Binder.Prefix can be removed it only serves as renaming the bexch
      --  which does not exist anymore.

      Add_Binder (PRA.Binder.Required_Switches, Lang_Ada_Idx);

      if Self.Ctxt.Is_Library and then Self.Ctxt.Is_Shared_Library then
         Add_Arg ("-shared");
      end if;

      Add_Arg ("-o");
      Add_Arg (String (Self.Output_Body.Path.Simple_Name));

      --  Directory separators are not allowed. We must be in the correct
      --  directory and only use the source base name with extension.

      for Ali of Self.Tree.Inputs (Self.UID, Explicit_Only => True) loop
         Add_Arg
           (GPR2.Build.Artifacts.Files.Object (Ali).Path.String_Value);
      end loop;

      for Extra of Self.Extra_Opts loop
         Add_Arg (Extra);
      end loop;

      for View of Self.Ctxt.Closure (True) loop
         if View.Is_Library and then View.Is_Library_Standalone then
            --  Force checking of elaboration flags
            Add_Arg ("-F");
            exit;
         end if;
      end loop;

      for View of Self.Ctxt.Closure loop
         --  Make sure all ALI directories are visible
         if View.Language_Ids.Contains (Ada_Language) then
            if View.Is_Library then
               Add_Arg ("-I" & View.Library_Ali_Directory.String_Value);
            elsif View.Kind in With_Object_Dir_Kind then
               Add_Arg ("-I" & View.Object_Directory.String_Value);
            end if;
         end if;
      end loop;

      --  [eng/gpr/gpr-issues#446] : This may include "-v"
      --  This switch is historically added for all GNAT version >= 6.4, it
      --  should be in the knowledge base instead of being hardcoded depending
      --  on the GNAT version.
      Status := Add_Attr_RS (PRA.Binder.Required_Switches, Lang_Ada_Idx);

      Status := Add_Attr (PRA.Binder.Switches, Lang_Ada_Idx, True);

      if not Status then
         Status := Add_Attr (PRA.Binder.Default_Switches, Lang_Ada_Idx, True);
      end if;

      --  Add -bargs and -bargs:Ada

      for Arg
        of Self.Tree.External_Options.Fetch
          (GPR2.External_Options.Binder, GPR2.No_Language)
      loop
         Add_Arg (Arg);
      end loop;

      for Arg
        of Self.Tree.External_Options.Fetch
          (GPR2.External_Options.Binder, GPR2.Ada_Language)
      loop
         Add_Arg (Arg);
      end loop;

      --  ??? Modify binding option -A=<file> if <file> is not an absolute path
      Enforce_DashA_Absolute_Path;

      --  [eng/gpr/gpr-issues#446]
      --  Should be in the KB Required_Switches/Default_Switches/Switches and
      --  not hardcoded.
      Add_Arg ("-x");

      Add_Mapping_File;

      if CL_Size > Command_Line_Limit then
         Create_Response_File;
      end if;
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self   : in out Object;
      Stdout : Unbounded_String;
      Stderr : Unbounded_String)
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      Self.Signature.Clear;

      for Pred of Self.Tree.Inputs (UID) loop
         Self.Signature.Add_Artifact (Pred);
      end loop;

      Self.Signature.Add_Artifact (Self.Generated_Spec);
      Self.Signature.Add_Artifact (Self.Generated_Body);

      Self.Signature.Add_Output (Stdout, Stderr);

      Self.Signature.Store (Self.Tree.Db_Filename_Path (UID));
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : in out Object;
      Basename   : Simple_Name;
      Context    : GPR2.Project.View.Object;
      Extra_Opts : GPR2.Containers.Value_List) is
   begin
      Self.Ctxt := Context;
      Self.Basename := +Basename;
      Self.Output_Spec :=
        Artifacts.Files.Create
          (Context.Object_Directory.Compose ("b__" & Basename & ".ads"));
      Self.Output_Body :=
        Artifacts.Files.Create
          (Context.Object_Directory.Compose ("b__" & Basename & ".adb"));
      Self.Traces := Create ("ACTION_ADA_BIND");

      for Opt of Extra_Opts loop
         Self.Extra_Opts.Append (Opt);
      end loop;

      Initialize_Linker_Options (Self);
   end Initialize;

   procedure Initialize
     (Self      : in out Object;
      Basename  : Simple_Name;
      Context   : GPR2.Project.View.Object;
      Extra_Opt : String := "")
   is
      Extra_Opts : GPR2.Containers.Value_List :=
                     GPR2.Containers.Empty_Value_List;
   begin
      if Extra_Opt'Length > 0 then
         Extra_Opts.Append (Extra_Opt);
      end if;

      Self.Initialize (Basename, Context, Extra_Opts);
   end Initialize;

   -------------------------------
   -- Initialize_Linker_Options --
   -------------------------------

   procedure Initialize_Linker_Options (Self : in out Object) is
      Idx  : PAI.Object;
      Attr : Project.Attribute.Object;
   begin
      if Self.Ctxt.Is_Library then
         if Self.Ctxt.Is_Shared_Library then
            Idx := PAI.Create ("-shared");
         else
            Idx := PAI.Create ("-static");
         end if;
      else
         return;
      end if;

      Attr :=
        Self.View.Attribute (PRA.Binder.Bindfile_Option_Substitution, Idx);

      if not Attr.Is_Defined then
         return;
      end if;

      for Value of Attr.Values loop
         Self.Linker_Opts.Append (Value.Text);
      end loop;
   end Initialize_Linker_Options;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID       : constant Actions.Action_Id'Class := Object'Class (Self).UID;
      Post_Bind : Actions.Post_Bind.Object;

   begin
      if not Db.Add_Output (UID, Self.Output_Spec)
         or else not Db.Add_Output (UID, Self.Output_Body)
      then
         return False;
      end if;

      Post_Bind :=
        Actions.Post_Bind.Create (Self.Output_Body, Self.View, Self);

      if not Db.Add_Action (Post_Bind) then
         return False;
      end if;

      Db.Add_Input (Post_Bind.UID, Self.Output_Body, True);

      return True;
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

   overriding function Post_Command
     (Self : in out Object; Status : Execution_Status) return Boolean
   is
      use Ada.Text_IO;
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Add_Remaining : Boolean := False;

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
            pragma Annotate (Xcov, Off, "unreachable code");
            raise Internal_Error
              with "Failed parsing line " & Line & " from " &
              Self.Output_Body.Path.String_Value;
            pragma Annotate (Xcov, On);
         end if;

         --  Skip the "--" comment prefix

         Switch_Index := Switch_Index + 2;

         declare
            Trimed_Line : constant String :=
                            Trim (Line (Switch_Index .. Line'Last), Both);
         begin
            if Trimed_Line (Trimed_Line'First) = '-' then
               Add_Remaining := True;
            end if;

            if Add_Remaining then
               Self.Linker_Opts.Append (Trimed_Line);
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

      return True;
   end Post_Command;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      BN     : constant Simple_Name := -Self.Basename;
      Result : constant Ada_Bind_Id :=
                 (Name_Len  => BN'Length,
                  BN        => BN,
                  Ctxt      => Self.Ctxt);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Ada_Bind;
