--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNAT.String_Split;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with GNATCOLL.Traces;
with GPR2.Build.Actions.Process.Link;
with GPR2.Message;
with GPR2.Project.Tree;

package body GPR2.Build.Actions.Process.Link_Options_Extract is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPR.BUILD.ACTIONS.LINK_OPTIONS_EXTRACT", GNATCOLL.Traces.Off);

   Blanks : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set
       (' ' & ASCII.HT & ASCII.CR & ASCII.LF & ASCII.NUL);
   --  Characters stripped from the ends of an extracted linker option. It
   --  includes the carriage return so CRLF line endings (Windows) do not leave
   --  a trailing CR, and NUL so the zero padding objcopy appends to a COFF
   --  section (Windows) does not leave a trailing NUL on an option (which
   --  would make e.g. "-lgnarl" become "-lgnarl\0" and fail to be found).

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
      Cmd_Line.Set_Driver
        (Self.View.Compiler_Prefix & "objdump");
      Cmd_Line.Add_Argument ("-s");
      Cmd_Line.Add_Argument ("--section=.GPR.linker_options");

      if Self.From_Archive then
         --  Scan the whole archive: objdump reads every member and reports
         --  the section from whichever one carries it. The archive may live
         --  outside the working directory, so pass its full path.

         Cmd_Line.Add_Argument (Self.Archive.Path.String_Value);
      else
         Cmd_Line.Add_Argument (String (Self.Object_File.Path.Simple_Name));
      end if;
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      if Self.From_Archive then
         if not Self.Signature.Add_Input (Self.Archive, Check_Checksums) then
            return;
         end if;
      else
         if not Self.Signature.Add_Input (Self.Object_File, Check_Checksums)
         then
            return;
         end if;
      end if;
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : in out Object;
      Object_File : Simple_Name;
      View        : GPR2.Project.View.Object) is
   begin
      --  Ensure the object wasn't previously initialized prior to this call
      Self := Undefined;

      Self.Ctxt := View;
      Self.Object_File :=
        GPR2.Build.Artifacts.Object_File.Create (Object_File);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Object;
      Archive : GPR2.Build.Artifacts.Library.Object;
      View    : GPR2.Project.View.Object) is
   begin
      --  Ensure the object wasn't previously initialized prior to this call
      Self := Undefined;

      Self.Ctxt         := View;
      Self.Archive      := Archive;
      Self.From_Archive := True;
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
      if Self.From_Archive then
         Db.Add_Input (UID, Self.Archive);
      else
         Db.Add_Input (UID, Self.Object_File);
      end if;

      return True;
   end On_Tree_Insertion;

   --------------------
   -- Post_Execution --
   --------------------

   overriding
   function Post_Execution
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean
   is
      function Decode_Stdout return String;
      --  Decode the content returned by the `objdump -s --section` command

      procedure Pass_Options_To_Linkers (Options : String);
      --  Split the provided options and pass them to the linker that depend
      --  on this action.

      -------------------
      -- Decode_Stdout --
      -------------------

      function Decode_Stdout return String is
         function Is_Hex (Str : String) return Boolean
         is (for all Char of Str => Char in '0' .. '9' | 'a' .. 'f');

         use GNAT.String_Split;

         Result : Unbounded_String;

         Separators            : constant String := ASCII.LF & "";
         Sliced_Linker_Options : Slice_Set;
         Current               : Positive;
         First_Content_Idx     : constant Positive := 6;
         Maximum_Content_Idx   : constant Positive := 40;
      begin

         Create
           (S          => Sliced_Linker_Options,
            From       => To_String (Stdout),
            Separators => Separators,
            Mode       => Multiple);

         for I in 1 .. Slice_Count (Sliced_Linker_Options) loop
            declare
               Line : constant String :=
                 Ada.Strings.Fixed.Trim
                   (Slice (Sliced_Linker_Options, I), Ada.Strings.Both);
            begin
               --  Only process lines that contain data
               if Line'Length > 42
                 and then Is_Hex (Line (1 .. 4))
                 and then (for all J in 9 .. Maximum_Content_Idx
                           => Line (J) in ' ' | '0' .. '9' | 'a' .. 'f')
                 and then Line (4 .. 5) = "0 "
                 and then Line (14) = ' '
                 and then Line (23) = ' '
                 and then Line (32) = ' '
                 and then Line
                            (Maximum_Content_Idx
                             + 1
                             .. Maximum_Content_Idx + 2)
                          = "  "
               then

                  --  Set the first character to the first data character of
                  --  the line.

                  Current := First_Content_Idx;

                  while Current <= Maximum_Content_Idx loop
                     Append
                       (Result,
                        Character'Val
                          (Integer'Value
                             ("16#" & Line (Current .. Current + 1) & '#')));
                     Current := Current + 2;
                     if Line (Current) = ' ' then
                        Current := Current + 1;

                        --  Two consecutive spaces indicate the end of the
                        --  data. These spaces then pad the line before the
                        --  ASCII column is displayed.

                        if Line (Current) = ' '
                          and then Current <= Maximum_Content_Idx
                        then
                           exit;
                        end if;
                     end if;
                  end loop;
               end if;
            end;
         end loop;

         return To_String (Result);
      end Decode_Stdout;

      -----------------------------
      -- Pass_Options_To_Linkers --
      -----------------------------

      procedure Pass_Options_To_Linkers (Options : String) is
         use GNAT.String_Split;

         Sliced_Options : Slice_Set;
         Separators     : constant String := ASCII.LF & "";
         Linkers_UID    : Action_Id_Sets.Set := Action_Id_Sets.Empty_Set;
      begin

         for Act of Self.Tree.Successors (Self.UID_Artifact) loop
            if Act in Link.Object'Class then
               GNATCOLL.Traces.Trace
                 (Traces,
                  "Inserting "
                  & Act.UID.Image
                  & " to the list of actions requiring linker options from "
                  & Self.UID.Image);
               Linkers_UID.Insert (Act.UID);
            end if;
         end loop;

         if Linkers_UID.Is_Empty then
            Self.Tree.Reporter.Report
              ("cannot find any linker related to action """
               & Self.UID.Image
               & '"',
               To_Stderr => True,
               Level     => GPR2.Message.Important);
            return;
         end if;

         Create
           (S          => Sliced_Options,
            From       => Options,
            Separators => Separators,
            Mode       => Multiple);

         for I in 1 .. Slice_Count (Sliced_Options) loop
            declare
               Raw : constant String :=
                 Ada.Strings.Fixed.Trim
                   (Slice (Sliced_Options, I),
                    Left  => Blanks,
                    Right => Blanks);

               Opt          : Unbounded_String := To_Unbounded_String (Raw);
               Archive_Ctxt : GPR2.Project.View.Object;

               use GPR2.Project;
            begin
               if Self.From_Archive then
                  declare
                     Static_Libs : Boolean := True;
                     Adalib_Dir  : constant GPR2.Path_Name.Object :=
                       (if Self.View.Tree.Runtime_Project.Is_Defined
                        then Self.View.Tree.Runtime_Project.Object_Directory
                        else GPR2.Path_Name.Undefined);
                  begin

                     --  Linker options insertion already transforms -lgnat and
                     --  -lgnarl options as absolute path to the runtime
                     --  archives. However, if linker options are coming from a
                     --  library that has been built with gprbuild1, the linker
                     --  options may contain raw -lgnat and -lgnarl that need
                     --  to be processed.

                     pragma Assert (Self.Tree.Has_Predecessor (Self.Archive));
                     pragma Assert (Adalib_Dir.Is_Defined);

                     Archive_Ctxt := Self.Tree.Predecessor (Self.Archive).View;

                     if Raw = "-shared" then
                        Static_Libs := False;
                     end if;

                     if Raw = "-lgnat" then
                        if Static_Libs
                          and then Archive_Ctxt.Library_Support /= None
                        then
                           if Archive_Ctxt.Is_Library
                             and then
                               Archive_Ctxt.Library_Standalone
                               = GPR2.Project.Encapsulated
                           then
                              Opt :=
                                To_Unbounded_String
                                  (Adalib_Dir.Compose ("libgnat_pic.a")
                                     .String_Value);
                           else
                              Opt :=
                                To_Unbounded_String
                                  (Adalib_Dir.Compose ("libgnat.a")
                                     .String_Value);
                           end if;
                        else
                           Opt := To_Unbounded_String (Raw);
                        end if;

                     elsif Raw = "-lgnarl" then
                        if Static_Libs
                          and then Archive_Ctxt.Library_Support /= None
                        then
                           if Archive_Ctxt.Is_Library
                             and then
                               Archive_Ctxt.Library_Standalone
                               = GPR2.Project.Encapsulated
                           then
                              Opt :=
                                To_Unbounded_String
                                  (Adalib_Dir.Compose ("libgnarl_pic.a")
                                     .String_Value);
                           else
                              Opt :=
                                To_Unbounded_String
                                  (Adalib_Dir.Compose ("libgnarl.a")
                                     .String_Value);
                           end if;
                        else
                           Opt := To_Unbounded_String (Raw);
                        end if;
                     end if;
                  end;
               end if;

               if To_String (Opt) /= "" then
                  for Linker_UID of Linkers_UID loop
                     Link.Object'Class
                       (Self.Tree.Action_Id_To_Reference (Linker_UID)
                          .Element.all)
                       .Add_Option_From_Binder (To_String (Opt));
                     Traces.Trace
                       ("Options "
                        & To_String (Opt)
                        & " passed to "
                        & Linker_UID.Image
                        & ":");
                  end loop;
               end if;
            end;
         end loop;
      end Pass_Options_To_Linkers;
   begin
      Pass_Options_To_Linkers (Decode_Stdout);

      return True;
   end Post_Execution;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      BN     : constant Simple_Name :=
                 (if Self.From_Archive
                  then Self.Archive.Path.Simple_Name
                  else Self.Object_File.Path.Simple_Name);
      Result : constant Link_Options_Extract_Id :=
        (Name_Len => BN'Length, View => Self.Ctxt, Object_File => BN);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Process.Link_Options_Extract;
