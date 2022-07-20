------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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
with Ada.Directories;
with Ada.Text_IO;

with GNAT.OS_Lib;

with GPR2.Version;

separate (GPRname.Options)
procedure Build_From_Command_Line (Self : in out Object) is

   use Ada.Command_Line;
   use Ada.Directories;

   subtype Arg_Access is String_Access
     with Dynamic_Predicate => Arg_Access = null
     or else (Arg_Access'Length > 0 and then Arg_Access'First = 1);

   Arg : Arg_Access;
   Pos : Integer := 1;

   Root_Prj : Unbounded_String; -- Project root taken from -P switch

   Current_Section : Section.Object := Empty_Section;

   Version_Displayed : Boolean := False;

   procedure Add_Directory (Name : String);
   --  Add directory to the Current_Section

   procedure Prepare_And_Add_Section (S : in out Section.Object);
   --  Make sure that section S is valid (i.e. contains at least one naming
   --  pattern) and add it to the gprname options (Self).

   procedure Read_Next_Arg
     (Err_Mssg : String := ""; Dash_Allowed : Boolean := True);
   --  Move forward in the argument list.
   --  Err_Mssg is displayed as failure if there is no more argument, or if
   --  Dash_Allowed is False and the next argument starts with a '-'.

   procedure Usage;
   --  Print usage

   procedure Show_Version;
   --  Print version if not already done

   -------------------
   -- Add_Directory --
   -------------------

   procedure Add_Directory (Name : String) is
   begin
      Current_Section.Add_Directory (Name, To_String (Root_Prj));
   end Add_Directory;

   -----------------------------
   -- Prepare_And_Add_Section --
   -----------------------------

   procedure Prepare_And_Add_Section (S : in out Section.Object) is
   begin
      if S.Is_Valid then
         S.Prepare (To_String (Root_Prj));
         Self.Sections.Append (S);

      else
         raise GPRname_Exception with "invalid section";
      end if;
   end Prepare_And_Add_Section;

   -------------------
   -- Read_Next_Arg --
   -------------------

   procedure Read_Next_Arg
     (Err_Mssg : String := ""; Dash_Allowed : Boolean := True) is
   begin
      if Pos < Argument_Count then
         Free (Arg);
         Pos := Pos + 1;
         Arg := new String'(Argument (Pos));

         if not Dash_Allowed and then Arg (1) = '-' then
            raise GPRname_Exception with Err_Mssg;
         end if;

      else
         raise GPRname_Exception with Err_Mssg;
      end if;
   end Read_Next_Arg;

   ------------------
   -- Show_Version --
   ------------------

   procedure Show_Version is
   begin
      if not Version_Displayed then
         GPR2.Version.Display
           ("GPRNAME", "2018", Version_String => GPR2.Version.Long_Value);
         Version_Displayed := True;
      end if;
   end Show_Version;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      use Ada.Text_IO;
   begin
      Put_Line ("Usage: gprname [switches] naming-pattern [naming-patterns]");
      Put_Line ("   {--and [switches] naming-pattern [naming-patterns]}");
      New_Line;

      Put_Line ("general switches:");

      Put_Line ("  --version   Display version and exit");
      Put_Line ("  -h, --help  Display usage and exit");
      New_Line;

      Put_Line ("  -eL             For backwards compatibility," &
                  " has no effect");
      Put_Line ("  -P[ ]<proj>     Update or create project file <proj>");
      New_Line;

      Put_Line ("  --ignore-duplicate-files    Ignore duplicate file names");
      Put_Line ("  --ignore-predefined-units   Ignore predefined units");
      New_Line;

      Put_Line ("  --no-backup     Do not create backup of project file");
      Put_Line ("  --target=<targ> Indicates the target of the GNAT compiler");
      Put_Line ("  --RTS=<dir>     Specify the Ada runtime");
      New_Line;

      Put_Line ("  -gnateD<sym>=<val> Preprocess with symbol definition");
      Put_Line ("  -gnatep=<data>     Preprocess files with data file");
      New_Line;

      Put_Line ("  --minimal-dirs  Keep as Source_Dirs only the directories" &
                  " that contain at least one source.");
      Put_Line ("                  This will also expand any /** suffix to" &
                  " an explicit list of directories.");
      New_Line;

      Put_Line ("  -v           Verbose output");
      Put_Line ("  -v -v        Very verbose output");
      Put_Line ("  -vP<x>       Specify verbosity when parsing Project Files" &
                  " (x = 0/1/2)");
      New_Line;

      Put_Line ("switches for naming pattern sections:");
      Put_Line ("  --and                Begin a new naming patterns section");
      Put_Line ("  -d[ ]<dir>           Use <dir> as one of the source" &
                  " directories for the current section");
      Put_Line ("  -D[ ]<file>          Get source directories and "
                & "files from <file>");
      Put_Line ("   [-x][ ]<pat>        [exclude] pattern <pat> " &
                  "for Ada source");
      Put_Line ("  -[x]f[ ]<pat>        [exclude] pattern <pat> " &
                  "for C source");
      Put_Line ("  -[x]f:<lang>[ ]<pat> [exclude] pattern <pat> " &
                  "for source of language <lang>");
      New_Line;
   end Usage;

begin

   --  Because of the specific CL processing needed in GPRname, we cannot use
   --  the current GNAT.Command_Line API.

   --  The following code is similar to the legacy Scan_Arg procedure, with
   --  some modifications aimed at having a less ambiguous behavior:
   --     - Leading dashes are allowed for any argument except the project file
   --       (to specify an Ada pattern starting with '-', use f:Ada '-...')
   --     - The foreign language pattern specifier is -f:<lang>, no whitespace
   --       allowed. It is consistent with the syntax for '=' (e.g. --RTS=...).
   --     - Support is added for foreign language excluded patterns.

   while Pos <= Argument_Count loop
      Arg := new String'(Argument (Pos));

      if Arg'Length >= 2 and then Arg (1 .. 2) = "-P" then
         if Self.Project_File /= No_String then
            raise GPRname_Exception with "only one -P switch may be specified";
         elsif Arg'Length = 2 then
            Read_Next_Arg (Err_Mssg => "project file name missing",
                           Dash_Allowed => False);
            Self.Project_File := +(Arg.all);
         else
            Self.Project_File := +(Arg (3 .. Arg'Last));
         end if;

         Root_Prj := +Containing_Directory (To_String (Self.Project_File));

      elsif Arg.all = "--ignore-predefined-units" then
         Self.Ignore_Predefined_Units := True;

      elsif Arg.all = "--ignore-duplicate-files" then
         Self.Ignore_Duplicate_Files := True;

      elsif Arg.all = "--no-backup" then
         Self.No_Backup := True;

      elsif Arg'Length >= 5 and then Arg (1 .. 5) = "--RTS" then
         if Self.RTS /= No_String then
            raise GPRname_Exception
              with "--RTS cannot be specified multiple times";
         elsif Arg'Length <= 6 or else Arg (6) /= '=' then
            raise GPRname_Exception with "missing path for --RTS";
         else
            Self.RTS := +Arg (7 .. Arg'Last);
         end if;

      elsif Arg'Length >= 8 and then Arg (1 .. 8) = "--target" then
         if Self.Target /= No_String then
            raise GPRname_Exception
              with "--target cannot be specified multiple times";
         elsif Arg'Length <= 9 or else Arg (9) /= '=' then
            raise GPRname_Exception with "missing path for --target";
         else
            Self.Target := +Arg (10 .. Arg'Last);
         end if;

      elsif Arg.all = "-eL" then
         Self.Follow_Symlinks := True;

      elsif Arg.all = "-v" then
         Show_Version;
         if Self.Verbosity = None then
            Self.Verbosity := Low;
         elsif Self.Verbosity = Low then
            Self.Verbosity := High;
         end if;

      elsif Arg'Length = 4 and then Arg (1 .. 3) = "-vP"
        and then Arg (4) in '0' .. '2'
      then
         Self.Verbose_Parsing := Integer'Value (Arg (4 .. 4));

      elsif Arg.all = "-h" or else Arg.all = "--help" then
         Usage;
         GNAT.OS_Lib.OS_Exit (0);

      elsif Arg.all = "--version" then
         Show_Version;
         GPR2.Version.Display_Free_Software;
         GNAT.OS_Lib.OS_Exit (0);

      elsif Arg.all = "--minimal-dirs" then
         Self.Minimal_Dirs := True;

      elsif Arg'Length >= 8 and then
        (Arg  (1 .. 7) = "-gnatep" or else Arg  (1 .. 7) = "-gnateD")
      then
         Self.Prep_Switches.Append (Arg.all);

      elsif Arg.all = "--and" then
         Prepare_And_Add_Section (Current_Section);
         Current_Section.Reset;

      elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-d" then
         if Arg'Length = 2 then
            Read_Next_Arg (Err_Mssg => "directory name missing");
            Add_Directory (Arg.all);
         else
            Add_Directory (Arg.all (3 .. Arg'Last));
         end if;

      elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-D" then
         if Arg'Length = 2 then
            Read_Next_Arg (Err_Mssg => "directories file name missing");
            Current_Section.Add_Directories_File (Arg.all);
         else
            Current_Section.Add_Directories_File (Arg.all (3 .. Arg'Last));
         end if;

      elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-f" then

         if Arg'Length = 2 then
            Read_Next_Arg (Err_Mssg => "pattern missing for language C");
            Current_Section.Add_Language_Pattern
              (Language_Type'("C"), Pattern_Type (Arg.all));

         elsif Arg (3) /= ':' then
            Current_Section.Add_Language_Pattern
              (Language_Type'("C"), Pattern_Type (Arg (3 .. Arg'Last)));

         elsif Arg'Length > 3 then
            declare
               Lang : constant String := Arg (4 .. Arg'Last);
            begin
               Read_Next_Arg
                 (Err_Mssg => "pattern missing for language " & Lang);
               Current_Section.Add_Language_Pattern
                 (Language_Type (Lang), Pattern_Type (Arg.all));
            end;

         else
            raise GPRname_Exception with "language missing for -f:";
         end if;

      elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-x" then

         if Arg'Length = 2 then
            Read_Next_Arg (Err_Mssg => "excluded pattern missing");
            Current_Section.Add_Excluded_Language_Pattern
              (Language_Type'("Ada"), Pattern_Type (Arg.all));

         elsif Arg (3) /= 'f' then
            Current_Section.Add_Excluded_Language_Pattern
              (Language_Type'("Ada"), Pattern_Type (Arg.all (3 .. Arg'Last)));

         elsif Arg'Length = 3 then
            Read_Next_Arg
              (Err_Mssg => "excluded pattern missing for language C");
            Current_Section.Add_Excluded_Language_Pattern
              (Language_Type'("C"), Pattern_Type (Arg.all));

         elsif Arg (4) /= ':' then
            Current_Section.Add_Excluded_Language_Pattern
              (Language_Type'("C"), Pattern_Type (Arg (4 .. Arg'Last)));

         elsif Arg'Length > 4 then
            declare
               Lang : constant String := Arg (5 .. Arg'Last);
            begin
               Read_Next_Arg
                 (Err_Mssg => "excluded pattern missing for language " & Lang);
               Current_Section.Add_Excluded_Language_Pattern
                 (Language_Type (Lang), Pattern_Type (Arg.all));
            end;
         else
            raise GPRname_Exception with "language missing for -xf:";
         end if;

      elsif Arg (1) = '-' then

         --  Any isolated argument starting with a dash is interpreted as a
         --  switch. To add a pattern starting with '-' for Ada, use the
         --  explicit form: -f:Ada <pat>

         raise GPRname_Exception with "wrong switch: " & Arg.all;

      else
         --  Pattern for Ada

         Current_Section.Add_Language_Pattern
           (Language_Type'("Ada"), Pattern_Type (Arg.all));
      end if;

      --  Go to the next argument

      Free (Arg);
      Pos := Pos + 1;
   end loop;

   --  Check that we have a (non-empty) project file name

   if Self.Project_File = No_String then
      raise GPRname_Exception with "project file name missing";
   end if;

   --  Add the last section on the command line

   Prepare_And_Add_Section (Current_Section);
end Build_From_Command_Line;
