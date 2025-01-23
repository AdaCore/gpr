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

with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Actions.Post_Bind;
pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Build.Tree_Db;
with GPR2.Build.External_Options;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
pragma Warnings (Off, "*is not referenced");
with GPR2.Project.View.Vector;
pragma Warnings (On);
with GPR2.Project.Tree;

package body GPR2.Build.Actions.Ada_Bind is

   use GNAT;

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self     : in out Object;
      Slot     : Positive;
      Cmd_Line : in out GPR2.Build.Command_Line.Object)
   is
      function Add_Attr
        (Id           : Q_Attribute_Id;
         Index        : PAI.Object;
         Is_List      : Boolean;
         In_Signature : Boolean) return Boolean;

      procedure Add_Binder (Id : Q_Attribute_Id; Index : PAI.Object);

      procedure Add_Mapping_File;

      procedure Create_Response_File;

      Lang_Ada_Idx : constant PAI.Object := PAI.Create (GPR2.Ada_Language);
      Status       : Boolean;

      --------------
      -- Add_Attr --
      --------------

      function Add_Attr
        (Id           : Q_Attribute_Id;
         Index        : PAI.Object;
         Is_List      : Boolean;
         In_Signature : Boolean) return Boolean
      is
         Attr                  : constant Project.Attribute.Object :=
                                   Self.View.Attribute (Id, Index);
         Gnatbind_Prefix_Equal : constant String := "gnatbind_prefix=";
         Gnatbind_Path_Equal   : constant String := "--gnatbind_path=";
         Ada_Binder_Equal      : constant String := "ada_binder=";

      begin
         if not Attr.Is_Defined then
            return False;
         end if;

         if Is_List then
            for Val of Attr.Values loop
               if Val.Text'Length > 0
                 and then not Starts_With (Val.Text, Gnatbind_Path_Equal)
                 and then not Starts_With (Val.Text, Gnatbind_Prefix_Equal)
                 and then not Starts_With (Val.Text, Ada_Binder_Equal)
                 --  Ignore -C, as the generated sources are always in Ada
                 and then Val.Text /= "-C"
               then
                  Cmd_Line.Add_Argument (Val.Text, In_Signature);
               end if;
            end loop;
         else
            Cmd_Line.Add_Argument (Attr.Value.Text, In_Signature);
         end if;

         return True;
      end Add_Attr;

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

         Cmd_Line.Add_Argument (Normalized_True_Binder_Path.all, True);

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
                            ("mapping", Local);
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
            if not Self.Ctxt.Is_Library
              or else not Self.Ctxt.Has_Any_Interfaces
            then
               for V of Self.View.Closure (True, False, False) loop
                  if not V.Is_Runtime then
                     for CU of V.Own_Units loop
                        declare
                           A_Comp : Compile.Ada.Object;
                           Key    : constant String :=
                                      To_Lower (String (CU.Name)) &
                                      (if CU.Main_Part = S_Spec
                                       then S_Suffix else B_Suffix);
                        begin
                           A_Comp.Initialize (CU);

                           Write
                             (Map_File.FD,
                              Key & ASCII.LF &
                                String (A_Comp.Ali_File.Path.Simple_Name) &
                                ASCII.LF &
                                A_Comp.Ali_File.Path.String_Value & ASCII.LF);
                        end;
                     end loop;
                  end if;
               end loop;

            else
               --  For standalone libraries we just use the main part of the
               --  interface.
               --  ??? Need to check what gprlib actually does.

               for CU of Self.Ctxt.Interface_Closure loop
                  --  Find the compile action corresponding to the unit
                  declare
                     A_Comp : Compile.Ada.Object;
                     Key    : constant String :=
                                To_Lower (String (CU.Name)) &
                                (if CU.Main_Part = S_Spec
                                 then S_Suffix else B_Suffix);
                  begin
                     A_Comp.Initialize (CU);

                     --  In case of Standalone library, we need to reference
                     --  at bind time the ALI in the object directory (the
                     --  one straight from the compiler) instead of the one
                     --  in the Library ali directory (with the SP flag set).

                     Write
                       (Map_File.FD,
                        Key & ASCII.LF &
                          String (A_Comp.Ali_File.Path.Simple_Name) &
                          ASCII.LF &
                          Self.Ctxt.Object_Directory.Compose
                          (A_Comp.Ali_File.Path.Simple_Name).String_Value &
                          ASCII.LF);
                  end;
               end loop;
            end if;

            GNATCOLL.OS.FS.Close (Map_File.FD);
         end if;

         Cmd_Line.Add_Argument ("-F=" & String (Map_File.Path), False);
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
         New_Args.Append (Cmd_Line.Argument_List.First_Element);

         if Resp_File.FD /= Null_FD then
            for Arg of Cmd_Line.Argument_List loop
               if Arg /= New_Args.First_Element then
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

         Cmd_Line.Set_Response_File_Command (New_Args);
      end Create_Response_File;

   begin
      --  [eng/gpr/gpr-issues#446] We should rework how the binder tools is
      --  fetched from the KB.
      --  Right now, gnatbind is called no matter what and
      --  Binder.Required_Switches is parsed to fetch gnatbind prefix or path.
      --  The binder tool should simply be stored in Binder.Driver
      --  Binder.Prefix can be removed it only serves as renaming the bexch
      --  which does not exist anymore.

      Add_Binder (PRA.Binder.Required_Switches, Lang_Ada_Idx);

      if not Self.Has_Main then
         Cmd_Line.Add_Argument ("-n");
      end if;

      if Self.SAL_Closure then
         Cmd_Line.Add_Argument ("-F");
      end if;

      Cmd_Line.Add_Argument ("-o", True);
      Cmd_Line.Add_Argument
        (String (Self.Output_Body.Path.Simple_Name), True);

      if Self.Ctxt.Is_Library then
         if Self.Ctxt.Is_Shared_Library then

            --  Link against a shared GNAT run time

            Cmd_Line.Add_Argument ("-shared", True);
         end if;

         if Self.Ctxt.Is_Library_Standalone then
            --  We need to specify the prefix for the init/final procedures.
            --  Make sure that the init procedure is never "adainit".

            if Self.Ctxt.Library_Name = "ada" then
               Cmd_Line.Add_Argument ("-Lada_", True);
            else
               Cmd_Line.Add_Argument
                 ("-L" & String (Self.Ctxt.Library_Name), True);
            end if;
         end if;

      else

         --  If at least one imported library is shared, gnatbind must also be
         --  called with the -shared flag.

         for Imported_View of Self.Ctxt.Imports (Recursive => True) loop
            if Imported_View.Is_Library
              and then Imported_View.Is_Shared_Library
            then
               Cmd_Line.Add_Argument ("-shared", True);
               exit;
            end if;
         end loop;
      end if;

      if Self.Ctxt.Is_Library
        and then Self.Ctxt.Is_Library_Standalone
      then
         Cmd_Line.Add_Argument ("-a");
      end if;

      for Ali of Self.Tree.Inputs (Self.UID, Explicit_Only => True) loop
         Cmd_Line.Add_Argument
           (GPR2.Build.Artifacts.Files.Object (Ali).Path, True);
      end loop;

      for View of Self.Ctxt.Closure (True, True, True) loop
         --  Make sure all ALI directories are visible
         if View.Language_Ids.Contains (Ada_Language) then
            if View.Is_Library then
               Cmd_Line.Add_Argument
                 ("-I" & View.Library_Ali_Directory.String_Value, True);

               if View.Id = Self.Ctxt.Id
                 and then Self.Ctxt.Is_Library_Standalone
               then
                  --  Need visibility on non-interface units

                  Cmd_Line.Add_Argument
                    ("-I" & View.Object_Directory.String_Value, True);
               end if;

            elsif View.Kind in With_Object_Dir_Kind then
               Cmd_Line.Add_Argument
                 ("-I" & View.Object_Directory.String_Value, True);
            end if;
         end if;
      end loop;

      --  [eng/gpr/gpr-issues#446] : This may include "-v"
      --  This switch is historically added for all GNAT version >= 6.4, it
      --  should be in the knowledge base instead of being hardcoded depending
      --  on the GNAT version.
      Status :=
        Add_Attr (PRA.Binder.Required_Switches, Lang_Ada_Idx, True, True);
      Status := Add_Attr (PRA.Binder.Switches, Lang_Ada_Idx, True, True);

      if not Status then
         Status :=
           Add_Attr (PRA.Binder.Default_Switches, Lang_Ada_Idx, True, True);
      end if;

      --  Add -bargs and -bargs:Ada

      for Arg of Self.Tree.External_Options.Fetch
                   (External_Options.Binder, GPR2.Ada_Language)
      loop
         Cmd_Line.Add_Argument (Arg);
      end loop;

      --  ??? Modify binding option -A=<file> if <file> is not an absolute path
      --  Enforce_DashA_Absolute_Path;

      --  [eng/gpr/gpr-issues#446]
      --  Should be in the KB Required_Switches/Default_Switches/Switches and
      --  not hardcoded.
      if not (Self.Ctxt.Is_Library
              and then Self.Ctxt.Is_Library_Standalone)
      then
         Cmd_Line.Add_Argument ("-x");
      end if;

      Add_Mapping_File;

      if Cmd_Line.Total_Length > Command_Line_Limit then
         Create_Response_File;
      end if;
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self      : Object;
      Signature : in out GPR2.Build.Signature.Object)
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      for Pred of Self.Tree.Inputs (UID) loop
         Signature.Add_Artifact (Pred);
      end loop;

      Signature.Add_Artifact (Self.Generated_Spec);
      Signature.Add_Artifact (Self.Generated_Body);
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self           : in out Object;
      Basename       : Simple_Name;
      Context        : GPR2.Project.View.Object;
      Has_Main       : Boolean;
      SAL_In_Closure : Boolean) is
   begin
      Self.Ctxt        := Context;
      Self.Basename    := +Basename;
      Self.Has_Main    := Has_Main;
      Self.SAL_Closure := SAL_In_Closure;
      Self.Output_Spec :=
        Artifacts.Files.Create
          (Context.Object_Directory.Compose ("b__" & Basename & ".ads"));
      Self.Output_Body :=
        Artifacts.Files.Create
          (Context.Object_Directory.Compose ("b__" & Basename & ".adb"));
      Self.Traces := Create ("ACTION_ADA_BIND");
   end Initialize;

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

      Runtime          : constant GPR2.Project.View.Object :=
                           Self.View.Tree.Runtime_Project;

      Add_Remaining    : Boolean := False;
      Xlinker_Seen     : Boolean := False;
      Stack_Equal_Seen : Boolean := False;
      Static_Libs      : Boolean := True;
      Adalib_Dir       : constant Path_Name.Object :=
                           Runtime.Object_Directory;

      procedure Process_Option_Or_Object_Line (Line : String);
      --  Pass options to the linker. Do not pass object file lines,
      --  as the objects to link are already obtained by parsing ALI files.

      -----------------------------------
      -- Process_Option_Or_Object_Line --
      -----------------------------------

      procedure Process_Option_Or_Object_Line (Line : String) is
         Idx              : constant PAI.Object :=
                              PAI.Create (Line, Case_Sensitive => True);
         Attr             : constant GPR2.Project.Attribute.Object :=
                              Self.View.Attribute
                                (PRA.Binder.Bindfile_Option_Substitution, Idx);
      begin
         if not Add_Remaining and then Line (Line'First) = '-' then
            --  We skip the list of objects, not reliable, the Tree_Db
            --  is more reliable. After the first option we however need
            --  to take everything since pragma Linker_Options may be
            --  anything, including non-options
            Add_Remaining := True;
         end if;

         if Line = "-shared" then
            Static_Libs := False;
         end if;

         if not Add_Remaining
           and then Path_Name.Simple_Name
             (Filename_Type (Line)) not in "g-trasym.o" | "g-trasym.obj"
         then
            --  g-trasym is a special case as it is not included in libgnat
            return;
         end if;

         if Line = "-Xlinker" then
            Xlinker_Seen := True;

         elsif Xlinker_Seen then
            Xlinker_Seen := False;

            --  Make sure that only the first switch --stack= is taken into
            --  account
            if Starts_With (Line, "-stack=") then
               if not Stack_Equal_Seen then
                  Stack_Equal_Seen := True;
                  Self.Linker_Opts.Append ("-Xlinker");
                  Self.Linker_Opts.Append (Line);
               end if;
            else
               Self.Linker_Opts.Append ("-Xlinker");
               Self.Linker_Opts.Append (Line);
            end if;

         elsif Starts_With (Line, "-Wl,--stack=") then
            if not Stack_Equal_Seen then
               Stack_Equal_Seen := True;
               Self.Linker_Opts.Append (Line);
            end if;

         elsif Attr.Is_Defined then
            for V of Attr.Values loop
               Self.Linker_Opts.Append (V.Text);
            end loop;

            --  For a number of archives, we need to indicate the full path of
            --  the arghive, if we find it, to be sure that the correct
            --  archive is used by the linker.

         elsif Line = "-lgnat" then
            if Static_Libs then
               Self.Linker_Opts.Append
                 (Adalib_Dir.Compose ("libgnat.a").String_Value);
            else
               Self.Linker_Opts.Append (Line);
            end if;

         elsif Line = "-lgnarl" then
            if Static_Libs then
               Self.Linker_Opts.Append
                 (Adalib_Dir.Compose ("libgnarl.a").String_Value);
            else
               Self.Linker_Opts.Append (Line);
            end if;
            --  ??? There are other libs to look for

         else
            Self.Linker_Opts.Append (Line);
         end if;
      end Process_Option_Or_Object_Line;

      Src_File     : File_Type;
      Reading      : Boolean         := False;
      Begin_Marker : constant String := "--  BEGIN Object file/option list";
      End_Marker   : constant String := "--  END Object file/option list";
      Switch_Index : Natural;

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
               Switch_Index := Index (Line, "--");

               if Switch_Index = 0 then
                  pragma Annotate (Xcov, Exempt_On, "unreachable code");
                  raise Internal_Error
                    with "Failed parsing line " & Line & " from " &
                    Self.Output_Body.Path.String_Value;
                  pragma Annotate (Xcov, Exempt_Off);
               end if;

               --  Skip the "--" comment prefix
               Switch_Index := Switch_Index + 2;
               Process_Option_Or_Object_Line
                 (Trim (Line (Switch_Index .. Line'Last), Both));
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
