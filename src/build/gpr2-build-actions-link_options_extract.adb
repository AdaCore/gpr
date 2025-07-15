--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNAT.String_Split;
with Ada.Strings.Fixed;
with GNATCOLL.Traces;
with GPR2.Build.Actions.Link;
with GPR2.Message;

package body GPR2.Build.Actions.Link_Options_Extract is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPR.BUILD.ACTIONS.LINK_OPTIONS_EXTRACT", GNATCOLL.Traces.Off);

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
      Cmd_Line.Add_Argument (String (Self.Object_File.Path.Simple_Name));
   end Compute_Command;

   ----------------------------
   -- Compute_Response_Files --
   ----------------------------

   overriding procedure Compute_Response_Files
     (Self           : in out Object;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean) is
   begin
      null;
   end Compute_Response_Files;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature (Self : in out Object; Load_Mode : Boolean) is
   begin
      if not Self.Signature.Add_Input (Self.Object_File) and then Load_Mode
      then
         return;
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
      Self.Ctxt := View;
      Self.Object_File :=
        GPR2.Build.Artifacts.Object_File.Create (Object_File);
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
      Db.Add_Input (UID, Self.Object_File, True);
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
            if Act in GPR2.Build.Actions.Link.Object'Class then
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
               Opt : constant String :=
                 Ada.Strings.Fixed.Trim
                   (Slice (Sliced_Options, I), Ada.Strings.Both);
            begin
               if Opt /= "" then
                  for Linker_UID of Linkers_UID loop
                     Link.Object'Class
                       (Self.Tree.Action_Id_To_Reference (Linker_UID)
                          .Element.all)
                       .Add_Option (Opt);
                     Traces.Trace
                       ("Options "
                        & Opt
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
   end Post_Command;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      BN     : constant Simple_Name := Self.Object_File.Path.Simple_Name;
      Result : constant Link_Options_Extract_Id :=
        (Name_Len => BN'Length, View => Self.Ctxt, Object_File => BN);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Link_Options_Extract;
