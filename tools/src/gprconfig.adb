------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with GNAT.Command_Line;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.String_Split;

with GNATCOLL.Traces;

with GPR2.Containers;
with GPR2.KB;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name.Set;
with GPR2.Project.Configuration;
with GPR2.Version;

with GPRtools.Util;

procedure GPRconfig is

   use Ada;
   use Ada.Containers;
   use Ada.Strings.Unbounded;

   use GNAT.Command_Line;

   use GPR2;
   use GPR2.KB;
   use GPR2.Project.Configuration;

   Knowledge_Base : KB.Object;

   type Verbosity_Kind is (Quiet, Default, Verbose);

   Opt_Verbosity    : Verbosity_Kind := Default;
   Opt_Version      : aliased Boolean;
   Opt_Target       : aliased GNAT.OS_Lib.String_Access;
   Opt_Show_Targets : aliased Boolean;
   Opt_Show_MI      : aliased Boolean;
   Opt_Show_Known   : aliased Boolean;
   Opt_Batch        : aliased Boolean;
   Opt_O            : aliased GNAT.OS_Lib.String_Access;
   Opt_DB           : Boolean := False;
   Opt_Validate     : aliased Boolean;
   Opt_Fallback     : aliased Boolean;

   KB_Flags         : Parsing_Flags := Default_Flags;

   Cmd_Config       : Command_Line_Configuration;

   KB_Locations     : GPR2.Path_Name.Set.Object;

   function "=" (L, R : Description) return Boolean is
     (Language (L) = Language (R));
   --  Compares descriptions. Only one description per language is expected

   package Description_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Language_Id, Description);

   Description_Map : Description_Maps.Map;

   procedure Register_Cmd_Options;
   --  Registers accepted gprconfig switches

   procedure Value_Callback (Switch, Value : String);
   --  Parses command line switches

   procedure Report_Error_And_Exit (Msg : String);
   --  Outputs error message and raises Exit_From_Command_Line

   function Parse_Config_Parameter (Config : String) return Description;
   --  Parses the value of --config switch into Decription

   function Get_Settings (Map : Description_Maps.Map) return Description_Set;
   --  Turns the map of processed config parameters into Description_Set

   procedure Display_Compilers_For_Parser
     (Base       : KB.Object;
      Compilers  : in out Compiler_Array;
      For_Target : Name_Type);
   --  Display the list of found compilers for use by an external parser

   procedure Select_Compilers_Interactively
     (Base       : in out KB.Object;
      Compilers  : in out Compiler_Array;
      For_Target : Name_Type);
   --  Asks the user for compilers to be selected

   procedure Show_Command_Line_Config
     (Compilers : Compiler_Array; Target : String);
   --  Displays the batch command line that would have the same effect as the
   --  current selection of compilers.

   ----------------------------------
   -- Display_Compilers_For_Parser --
   ----------------------------------

   procedure Display_Compilers_For_Parser
     (Base       : KB.Object;
      Compilers  : in out Compiler_Array;
      For_Target : Name_Type)
   is
      Comp   : Compiler;

      function "&" (L : String; R : Optional_Name_Type) return String is
        (L & String (R));

      procedure Put_Rank (Comp : Compiler; Idx : Positive);
      --  Outputs prefix with rank and selection

      --------------
      -- Put_Rank --
      --------------

      procedure Put_Rank (Comp : Compiler; Idx : Positive) is
      begin
         if Is_Selected (Comp) then
            Text_IO.Put ("*");
            Integer_Text_IO.Put (Idx, Width => 3);
         else
            Integer_Text_IO.Put (Idx, Width => 4);
         end if;
      end Put_Rank;

   begin
      Base.Filter_Compilers_List (Compilers, For_Target);

      for Idx in Compilers'Range loop

         Comp := Compilers (Idx);

         if Is_Selectable (Comp) and then Requires_Compiler (Comp) then
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" target:" & Target (Comp));
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line
              (" normalized_target:" & Base.Normalized_Target (Target (Comp)));
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" executable:" & Executable (Comp));
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" path:" & Path (Comp));
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" lang:" & Image (Language (Comp)));
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" name:" & Name (Comp));
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" version:" & KB.Version (Comp));
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" runtime:" & Runtime (Comp));
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line
              (" native:"
               & Boolean'Image
                 (Base.Normalized_Target (Default_Target) =
                      Base.Normalized_Target (Target (Comp))));

         elsif Is_Selectable (Comp) then

            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" target:");
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line
              (" normalized_target:unknown");
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" executable:");
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" path:");
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" lang:" & String (Name (Language (Comp))));
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" name:");
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" version:");
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" runtime:");
            Put_Rank (Comp, Idx);
            Text_IO.Put_Line (" native:FALSE");
         end if;

      end loop;
   end Display_Compilers_For_Parser;

   ------------------
   -- Get_Settings --
   ------------------

   function Get_Settings (Map : Description_Maps.Map) return Description_Set is
      Result : Description_Set (1 .. Integer (Map.Length));
      Idx    : Positive := 1;
   begin
      for Descr of Map loop
         Result (Idx) := Descr;
         Idx := Idx + 1;
      end loop;

      return Result;
   end Get_Settings;

   ----------------------------
   -- Parse_Config_Parameter --
   ----------------------------

   function Parse_Config_Parameter (Config : String) return Description is
      use Ada.Characters.Handling;
      use GNAT.String_Split;

      function Positional_Parameters return Boolean;
      --  Returns True if configuration parameters are given in a positional
      --  form,  i.e. --config=language:ada,runtime:sjlj.
      --  Also checks that the two modes are not mixed up,
      --  reports error otherwise.

      procedure Check_Positional;
      --  Checks that positional prefixes are not duplicated

      function Get_Description_Param
        (Slices : Slice_Set; Pos : Slice_Number) return Optional_Name_Type
      is
        (if not Has_Element (Slices, Pos) or else Slice (Slices, Pos) = ""
         then No_Name else Optional_Name_Type (Slice (Slices, Pos)));
      --  Returns parameter of configuration for given position or No_Name
      --  if corresponding position is empty/absent. Reports error ans fails
      --  if prefix is not followed by a value.

      function Get_Description_Param
        (Slices : Slice_Set; Prefix : String) return Optional_Name_Type;
      --  Returns parameter of configuration for given Prefix or No_Name
      --  if corresponding position is empty/absent.

      Slices : constant Slice_Set := Create (Config, ",");

      ----------------------
      -- Check_Positional --
      ----------------------

      procedure Check_Positional is
         use Ada.Strings.Fixed;

         Language_Set : Boolean := False;
         Version_Set  : Boolean := False;
         Runtime_Set  : Boolean := False;
         Path_Set     : Boolean := False;
         Name_Set     : Boolean := False;
      begin
         for Slice of Slices loop
            if To_Lower (Head (Slice, 9)) = "language:" then
               if Language_Set then
                  Report_Error_And_Exit
                    ("Configuration parameter language specified twice in "
                     & Config
                     & ASCII.LF
                     & "Invalid configuration specified with --config");
               else
                  Language_Set := True;
               end if;

            elsif To_Lower (Head (Slice, 8)) = "version:" then
               if Version_Set then
                  Report_Error_And_Exit
                    ("Configuration parameter version specified twice in "
                     & Config
                     & ASCII.LF
                     & "Invalid configuration specified with --config");
               else
                  Version_Set := True;
               end if;

            elsif To_Lower (Head (Slice, 8)) = "runtime:" then
               if Runtime_Set then
                  Report_Error_And_Exit
                    ("Configuration parameter runtime specified twice in "
                     & Config
                     & ASCII.LF
                     & "Invalid configuration specified with --config");
               else
                  Runtime_Set := True;
               end if;

            elsif To_Lower (Head (Slice, 5)) = "path:" then
               if Path_Set then
                  Report_Error_And_Exit
                    ("Configuration parameter path specified twice in "
                     & Config
                     & ASCII.LF
                     & "Invalid configuration specified with --config");
               else
                  Path_Set := True;
               end if;

            elsif To_Lower (Head (Slice, 5)) = "name:" then
               if Name_Set then
                  Report_Error_And_Exit
                    ("Configuration parameter name specified twice in "
                     & Config
                     & ASCII.LF
                     & "Invalid configuration specified with --config");
               else
                  Name_Set := True;
               end if;

            end if;
         end loop;
      end Check_Positional;

      ---------------------------
      -- Get_Description_Param --
      ---------------------------

      function Get_Description_Param
        (Slices : Slice_Set; Prefix : String) return Optional_Name_Type
      is
         use Ada.Strings.Fixed;

         Pref     : constant String := Prefix & ":";
         Pref_Len : constant Positive := Pref'Length;
      begin
         for Slice of Slices loop
            if To_Lower (Head (Slice, Pref_Len)) = Pref then
               if Slice = Pref then
                  Report_Error_And_Exit
                    ("Parameter value for " & Prefix & " not specified in """
                     & Config & """" & ASCII.LF
                     & "Invalid configuration specified with --config");
               end if;

               return
                 Optional_Name_Type
                   (Slice (Slice'First + Pref_Len .. Slice'Last));
            end if;
         end loop;

         return No_Name;
      end Get_Description_Param;

      ---------------------------
      -- Positional_Parameters --
      ---------------------------

      function Positional_Parameters return Boolean is
         use Ada.Strings.Fixed;

         Positional_Present     : Boolean := False;
         Not_Positional_Present : Boolean := False;
      begin
         for Slice of Slices loop
            if Slice = ""
              or else (To_Lower (Head (Slice, 9)) /= "language:"
                        and then To_Lower (Head (Slice, 8)) /= "version:"
                        and then To_Lower (Head (Slice, 8)) /= "runtime:"
                        and then To_Lower (Head (Slice, 5)) /= "path:"
                        and then To_Lower (Head (Slice, 5)) /= "name:")
            then
               Not_Positional_Present := True;
            else
               Positional_Present := True;
            end if;

            if Not_Positional_Present and then Positional_Present then
               Report_Error_And_Exit
                 ("Mixing positional and not positional parameters in """
                  & Config & """"
                  & ASCII.LF
                  & "Invalid configuration specified with --config");
            end if;
         end loop;

         return Positional_Present;
      end Positional_Parameters;

      Result : Description;
   begin
      if Slice_Count (Slices) > 5 then
         Report_Error_And_Exit
           ("Too many arguments in configuration """ & Config & """"
            & ASCII.LF
            & "Invalid configuration specified with --config");
      end if;

      if Positional_Parameters then
         Check_Positional;

         if Get_Description_Param (Slices, "language") = No_Name then
            Report_Error_And_Exit
              ("Language not specified if " & Config
               & ASCII.LF
               & "Invalid configuration specified with --config");
         end if;

         Result := Project.Configuration.Create
           (Language => +Get_Description_Param (Slices, "language"),
            Version  => Get_Description_Param (Slices, "version"),
            Runtime  => Get_Description_Param (Slices, "runtime"),
            Path     => Filename_Optional
                          (Get_Description_Param (Slices, "path")),
            Name     => Get_Description_Param (Slices, "name"));
      else
         if Get_Description_Param (Slices, 1) = No_Name then
            Report_Error_And_Exit
              ("Language not specified if " & Config
               & ASCII.LF
               & "Invalid configuration specified with --config");
         end if;

         Result := Project.Configuration.Create
           (Language => +Get_Description_Param (Slices, 1),
            Version  => Get_Description_Param (Slices, 2),
            Runtime  => Get_Description_Param (Slices, 3),
            Path     => Filename_Optional (Get_Description_Param (Slices, 4)),
            Name     => Get_Description_Param (Slices, 5));
      end if;

      return Result;
   end Parse_Config_Parameter;

   --------------------------
   -- Register_Cmd_Options --
   --------------------------

   procedure Register_Cmd_Options is
   begin
      Define_Switch
        (Cmd_Config, Opt_Version'Access,
         Long_Switch => "--version",
         Help        => "Display version and exit");
      Define_Switch
        (Cmd_Config,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");
      Define_Switch
        (Cmd_Config, Opt_Target'Access,
         Long_Switch => "--target=",
         Help        =>
           "Select specified target or ""all"" for any target" & ASCII.LF
         & "   ("
         & String (Default_Target)
         & " by default)",
         Argument    => "target");
      Define_Switch
        (Cmd_Config, Opt_Show_Targets'Access,
         Long_Switch => "--show-targets",
         Help        => "List all compiler targets available");
      Define_Switch
        (Cmd_Config, Opt_Show_MI'Access,
         Long_Switch => "--mi-show-compilers",
         Help        =>
           "List all compilers available in a parser-friendly way");
      Define_Switch
        (Cmd_Config, Opt_Show_Known'Access,
         Long_Switch => "--show-known-compilers",
         Help        =>
           "List names of all known compilers");
      Define_Switch
        (Cmd_Config, Opt_Batch'Access,
         Long_Switch => "--batch",
         Help        => "Batch mode, no interactive compiler selection");
      Define_Switch
        (Cmd_Config, Value_Callback'Unrestricted_Access,
         Switch => "-v",
         Help   => "Verbose mode");
      Define_Switch
        (Cmd_Config, Value_Callback'Unrestricted_Access,
         Switch => "-q",
         Help   => "Quiet mode");
      Define_Switch
        (Cmd_Config, Opt_O'Access,
         Switch   => "-o:",
         Help     =>
           "Name and directory of the output file"  & ASCII.LF
           & "  (default is default.cgpr)",
         Argument => "file");
      Define_Switch
        (Cmd_Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--db:",
         Help        =>
           "Parse DIR as an additional knowledge base" & ASCII.LF
           & " --db-                  Do not load the standard knowledge base",
         Argument    => "dir");
      --  ??? Do we still want to point to the KB in file format
      --  if it can be found?
      Define_Switch
        (Cmd_Config, Opt_Validate'Access,
         Long_Switch => "--validate",
         Help        =>
           "Validate contents of the knowledge base before loading");
      Define_Switch
        (Cmd_Config, Opt_Fallback'Access,
         Long_Switch => "--fallback-targets",
         Help        =>
           "Look for native toolchains of different architecture");
      Define_Switch
        (Cmd_Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--config=",
         Help        =>
           "Preselect a compiler."
           & ASCII.LF
           & "  CONFIG=language[,version[,runtime[,path[,name]]]]"
           & ASCII.LF
           & "  Name is either one of the names of the blocks"
           & " in the knowledge base"
           & ASCII.LF
           & "  ('GCC', 'GCC-28',...) or the base name of an executable"
           & " ('gcc', 'gnatmake')."
           & ASCII.LF
           & "  An empty string can be specified for any of the"
           & " optional parameters,"
           & ASCII.LF
           & "  otherwise positional prefixes can be used:"
           & ASCII.LF
           & "  --config=language:ada,runtime:zfp equals to "
           & "--config=ada,,zfp.",
         Argument    => "config");

      Set_Usage (Cmd_Config, Usage => "[switches]");

   end Register_Cmd_Options;

   ---------------------------
   -- Report_Error_And_Exit --
   ---------------------------

   procedure Report_Error_And_Exit (Msg : String) is
   begin
      Text_IO.Put_Line (Text_IO.Standard_Error, Msg);
      raise Exit_From_Command_Line;
   end Report_Error_And_Exit;

   ------------------------------------
   -- Select_Compilers_Interactively --
   ------------------------------------

   procedure Select_Compilers_Interactively
     (Base       : in out KB.Object;
      Compilers  : in out Compiler_Array;
      For_Target : Name_Type)
   is
      Input : Unbounded_String;
      Comp  : Compiler;
   begin
      loop
         Base.Filter_Compilers_List (Compilers, For_Target);

         Text_IO.Put_Line
           ("--------------------------------------------------");
         Text_IO.Put_Line
           ("gprconfig has found the following compilers on your PATH.");
         Text_IO.Put_Line
           ("Only those matching the target and the selected compilers"
            & " are displayed.");

         for Idx in Compilers'Range loop
            Comp := Compilers (Idx);

            if Is_Selectable (Comp) then

               if Is_Selected (Comp) then
                  Text_IO.Put ("*");
                  Integer_Text_IO.Put (Idx, Width => 3);
               else
                  Integer_Text_IO. Put (Idx, Width => 4);
               end if;

               Text_IO.Put (". ");

               if Requires_Compiler (Comp) then
                  Text_IO.Put
                    (String (Name (Comp))
                     & " for "
                     & Image (Language (Comp))
                     & " in "
                     & String (Path (Comp)));

                  if For_Target = "all" then
                     Text_IO.Put (" on " & String (Target (Comp)));
                  end if;

                  Text_IO.Put
                    (" version " & String (KB.Version (Comp)));

                  if Runtime (Comp, True) = No_Name then
                     Text_IO.New_Line;
                  else
                     Text_IO.Put_Line
                       (" ("
                        & String (Runtime (Comp, True))
                        & " runtime)");
                  end if;

               else
                  Text_IO.Put_Line
                    (Image (Language (Comp)) &
                       " (no compiler required)");
               end if;

            end if;
         end loop;

         Text_IO.Put
           ("Select or unselect the following compiler (or ""s"" to save): ");
         Input := To_Unbounded_String (Text_IO.Get_Line);

         exit when To_String (Input) = "s";

         declare
            Choice : Positive;
         begin
            Choice := Positive'Value (To_String (Input));

            if Choice > Compilers'Last then
               Text_IO.Put_Line ("Unrecognized choice");

            else
               if Is_Selected (Compilers (Choice)) then
                  Set_Selection (Compilers (Choice), False);
               else

                  Set_Selection (Compilers (Choice), True);
               end if;
            end if;

         exception
            when Constraint_Error =>
               Text_IO.Put_Line ("Unrecognized choice");
         end;

      end loop;
   end Select_Compilers_Interactively;

   ------------------------------
   -- Show_Command_Line_Config --
   ------------------------------

   procedure Show_Command_Line_Config
     (Compilers : Compiler_Array; Target : String) is
   begin
      if Compilers = No_Compilers then
         return;
      end if;

      Text_IO.New_Line;
      Text_IO.Put_Line
        ("You can regenerate the same config file in batch mode");
      Text_IO.Put_Line (" with the following command line:");
      Text_IO.Put ("gprconfig --batch");
      Text_IO.Put (" --target=");
      Text_IO.Put (Target);

      for Comp of Compilers loop
         if Is_Selected (Comp) then
            Text_IO.Put (" --config=");

            if Requires_Compiler (Comp) then
               Text_IO.Put
                 (Image (Language (Comp)) & ","
                  & String (KB.Version (Comp)) & ","
                  & String (Runtime (Comp)) & ","
                  & String (Path (Comp)) & ","
                  & String (Name (Comp)));
            else
               Text_IO.Put (Image (Language (Comp)) & ",,,,");
            end if;
         end if;
      end loop;

      Text_IO.New_Line;
      Text_IO.New_Line;
   end Show_Command_Line_Config;

   --------------------
   -- Value_Callback --
   --------------------

   procedure Value_Callback (Switch, Value : String) is
      Descr : Project.Configuration.Description;
   begin
      if Switch = "--db" then
         if Value = "-" then
            Opt_DB := True;

         else
            declare
               KB_Norm : constant String :=
                           GNAT.OS_Lib.Normalize_Pathname (Value);
               KB_Path : GPR2.Path_Name.Object;
            begin
               if GNAT.OS_Lib.Is_Directory (KB_Norm) then
                  KB_Path :=
                    GPR2.Path_Name.Create_Directory
                      (GPR2.Filename_Type (KB_Norm));

               elsif GNAT.OS_Lib.Is_Regular_File (KB_Norm) then
                  KB_Path :=
                    GPR2.Path_Name.Create_File (GPR2.Filename_Type (KB_Norm));

               else
                  Report_Error_And_Exit
                    (KB_Norm & " is not a file or directory");
               end if;

               KB_Locations.Append (KB_Path);
            end;
         end if;

      elsif Switch = "--config" then
         Descr := Parse_Config_Parameter (Value);
         if Description_Map.Contains (Language (Descr)) then
            Report_Error_And_Exit
              ("Multiple --config specified for " & Image (Language (Descr)));
         end if;
         Description_Map.Include (Language (Descr), Descr);

      elsif Switch = "-q" then
         Opt_Verbosity := Quiet;

      elsif Switch = "-v" then
         Opt_Verbosity := Verbose;

      end if;
   end Value_Callback;

   Config_Contents          : Unbounded_String;
   Selected_Target          : Unbounded_String;
   Output_File              : Unbounded_String;
   Config_Log               : Log.Object;
   Output                   : Text_IO.File_Type;
   Default_Config_File_Name : constant String := "default.cgpr";

begin
   begin
      GNATCOLL.Traces.Parse_Config_File;
   exception
      when E : others =>
         Text_IO.Put_Line
           (Text_IO.Standard_Error,
            "Cannot parse trace configuration file "
            & "(traces may work incorrectly):");
         Text_IO.Put_Line
           (Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Message (E));
   end;
   GPRtools.Util.Set_Program_Name ("gprconfig");

   Register_Cmd_Options;

   Getopt (Cmd_Config);

   if Opt_Version then
      GPR2.Version.Display
        ("GPRCONFIG", "2006",  Version_String => GPR2.Version.Long_Value);
      GPR2.Version.Display_Free_Software;
      return;
   end if;

   if Opt_Batch and then Opt_Target.all = "all" then
      if Opt_Verbosity > Quiet then
         Text_IO.Put_Line
           (Text_IO.Standard_Error,
            "--target=all ignored in --batch mode");
      end if;

      GNAT.OS_Lib.Free (Opt_Target);
      Opt_Target := new String'("");
   end if;

   KB_Flags (Validation) := Opt_Validate;

   if Opt_Verbosity = Verbose then
      GNATCOLL.Traces.Set_Active
        (GNATCOLL.Traces.Create
           ("KNOWLEDGE_BASE"), True);
      GNATCOLL.Traces.Set_Active
        (GNATCOLL.Traces.Create
           ("KNOWLEDGE_BASE.PARSING_TRACE"), True);
      GNATCOLL.Traces.Set_Active
        (GNATCOLL.Traces.Create
           ("KNOWLEDGE_BASE.MATHCING"), True);
      GNATCOLL.Traces.Set_Active
        (GNATCOLL.Traces.Create
           ("KNOWLEDGE_BASE.COMPILER_ITERATOR"), True);
   end if;

   if Opt_DB then
      Knowledge_Base := Create_Empty;
   else
      Knowledge_Base := Create_Default (KB_Flags);
   end if;

   for KB_Location of KB_Locations loop
      Knowledge_Base.Add (KB_Flags, KB_Location);
   end loop;

   for Msg_Cur in Knowledge_Base.Log_Messages.Iterate
     (Information => Opt_Verbosity = Verbose,
      Warning     => Opt_Verbosity = Verbose)
   loop
      Log.Element (Msg_Cur).Output;
   end loop;

   if Knowledge_Base.Has_Error then
      Text_IO.Put_Line
        (Text_IO.Standard_Error,
         "Invalid setup of the gprconfig knowledge base");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   if Opt_Show_Known then
      Text_IO.Put_Line
        ("The known compilers are: "
         & To_String (Knowledge_Base.Known_Compiler_Names));
      return;
   end if;

   if Opt_Target.all = "" then
      Selected_Target :=
        To_Unbounded_String
          (String
             (Knowledge_Base.Normalized_Target
                (Default_Target)));
   else
      Selected_Target := To_Unbounded_String (Opt_Target.all);
   end if;

   if Opt_O.all = "" then
      if Opt_Target.all = "" then
         Output_File := To_Unbounded_String (Default_Config_File_Name);
      else
         Output_File := To_Unbounded_String (Opt_Target.all & ".cgpr");
      end if;
   else
      Output_File := To_Unbounded_String (Opt_O.all);
   end if;

   if Opt_Batch then
      Config_Contents := Configuration
        (Self     => Knowledge_Base,
         Settings => Get_Settings (Description_Map),
         Target   => Name_Type (To_String (Selected_Target)),
         Messages => Config_Log,
         Fallback => Opt_Fallback);

   else
      if Opt_Show_Targets then
         Selected_Target := To_Unbounded_String ("all");
      end if;

      declare
         Compilers : Compiler_Array := Knowledge_Base.All_Compilers
           (Settings => Get_Settings (Description_Map),
            Target   => Name_Type (To_String (Selected_Target)),
            Messages => Config_Log);

         Set_Of_Targets : GPR2.Containers.Name_Set;
      begin
         if Knowledge_Base.Has_Error then
            for Msg_Cur in Knowledge_Base.Log_Messages.Iterate
              (Information => Opt_Verbosity > Quiet,
               Warning     => Opt_Verbosity > Quiet)
            loop
               Log.Element (Msg_Cur).Output;
            end loop;

            Text_IO.Put_Line
              (Text_IO.Standard_Error,
               "Invalid setup of the gprconfig knowledge base");
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         if Opt_Show_Targets or else Opt_Verbosity = Verbose then

            Text_IO.Put_Line ("List of targets supported by a compiler:");

            for Comp of Compilers loop
               Set_Of_Targets.Include
                 (Knowledge_Base.Normalized_Target (Target (Comp)));
            end loop;

            for Tgt of Set_Of_Targets loop

               Text_IO.Put (String (Tgt));

               if Tgt = Default_Target then
                  Text_IO.Put_Line (" (native target)");
               else
                  Text_IO.New_Line;
               end if;
            end loop;

         end if;

         if Opt_Show_Targets then
            return;
         end if;

         if Compilers'Length = 0 then
            if Selected_Target = Null_Unbounded_String then
               Text_IO.Put_Line
                 (Text_IO.Standard_Error,
                  "No compilers found for target "
                  & To_String (Selected_Target));
            else
               Text_IO.Put_Line
                 (Text_IO.Standard_Error, "No compilers found");
            end if;
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         if Opt_Show_MI then
            Display_Compilers_For_Parser
              (Knowledge_Base, Compilers,
               Name_Type (To_String (Selected_Target)));
            return;
         else
            Select_Compilers_Interactively
              (Knowledge_Base, Compilers,
               Name_Type (To_String (Selected_Target)));
            Show_Command_Line_Config (Compilers, To_String (Selected_Target));
         end if;

         Config_Contents := Configuration
           (Self      => Knowledge_Base,
            Selection => Compilers,
            Target    => Name_Type (To_String (Selected_Target)),
            Messages  => Config_Log);
      end;

   end if;

   if Config_Log.Has_Error then
      for Msg_Cur in Config_Log.Iterate
        (Information => Opt_Verbosity > Quiet,
         Warning     => Opt_Verbosity > Quiet,
         Read        => False)
      loop
         Log.Element (Msg_Cur).Output;
      end loop;

      Text_IO.Put_Line
        (Text_IO.Standard_Error,
         "Generation of configuration files failed");

      GNAT.OS_Lib.OS_Exit (1);

   elsif Knowledge_Base.Has_Error then

      for Msg_Cur in Knowledge_Base.Log_Messages.Iterate
        (Information => Opt_Verbosity > Quiet,
         Warning     => Opt_Verbosity > Quiet)
      loop
         Log.Element (Msg_Cur).Output;
      end loop;

      Text_IO.Put_Line
        (Text_IO.Standard_Error,
         "Invalid setup of the gprconfig knowledge base");

      GNAT.OS_Lib.OS_Exit (1);

   else
      for Msg_Cur in Config_Log.Iterate
        (Information => Opt_Verbosity > Quiet)
      loop
         Log.Element (Msg_Cur).Output;
      end loop;
   end if;

   if Config_Contents /= Null_Unbounded_String then
      Text_IO.Create
        (Output, Text_IO.Out_File, To_String (Output_File));

      Text_IO.Put_Line
        (Output, "--  This gpr configuration file was generated by gprconfig");
      Text_IO.Put_Line (Output, "--  using this command line:");
      Text_IO.Put (Output, "--  " & Ada.Command_Line.Command_Name);

      for I in 1 .. Ada.Command_Line.Argument_Count loop
         Text_IO.Put (Output, ' ');
         Text_IO.Put (Output, Ada.Command_Line.Argument (I));
      end loop;

      Text_IO.New_Line (Output);
      Text_IO.Put (Output, "--  from ");
      Text_IO.Put (Output, GNAT.Directory_Operations.Get_Current_Dir);
      Text_IO.New_Line (Output);

      Text_IO.Put_Line (Output, To_String (Config_Contents));
      Text_IO.Close (Output);
   end if;

exception
   when Ada.Directories.Name_Error | Ada.IO_Exceptions.Use_Error =>
      Text_IO.Put_Line
        (Text_IO.Standard_Error,
         "Could not create the file " & To_String (Output_File));

   when Invalid_Switch | Exit_From_Command_Line =>
      GNAT.OS_Lib.OS_Exit (1);

   when Invalid_Parameter =>
      Text_IO.Put_Line
        (Text_IO.Standard_Error,
         "Missing parameter for switch: -" & Full_Switch);
      Text_IO.Put_Line
        (Text_IO.Standard_Error,
         "try ""gprconfig --help"" for more information.");

   when E : others =>
      Text_IO.Put_Line
        (Text_IO.Standard_Error,
         "Unrecoverable error in GPRconfig: "
         & Ada.Exceptions.Exception_Information (E));
      GNAT.OS_Lib.OS_Exit (1);
end GPRconfig;
