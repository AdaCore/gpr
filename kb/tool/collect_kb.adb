------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                      Copyright (C) 2020-2024, AdaCore                    --
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

--  This utility collects all files of the GPRconfig Knowledge Base and
--  composes them into a single file intended for embedding in GPR2 library.

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.OS_Lib;

with Direct_IO;

function Collect_KB return Natural is

   use Ada;
   use Ada.Strings.Unbounded;
   use GNAT;

   Fail_Error  : exception;
   --  Raised to terminate execution

   Output_Unit : constant String := "GPR2.KB.Embedded";
   Output_Name : constant String := "gpr2-kb-embedded.adb";

   Output_File : Unbounded_String :=
                  To_Unbounded_String
                    (OS_Lib.Normalize_Pathname
                       (Output_Name,
                        Directory      => Directories.Current_Directory,
                        Case_Sensitive => False));
   --  Output file where composed knoledge base should be written

   KB_Dir      : Unbounded_String;
   --  Location of knowledge base

   Output      : Unbounded_String;

   procedure Add_File (Path : String);
   --  Add file to the generated output

   procedure Help;
   --  Displays help on using this application

   function To_String_Literal (S : String) return String;
   --  Translates S as an Ada string literal (so enclosed within double quotes
   --  and all internal double quotes are doubled).

   --------------
   -- Add_File --
   --------------

   procedure Add_File (Path : String) is
      KB_File_In    : Text_IO.File_Type;
      First_Literal : Boolean;
   begin
      Append (Output, ASCII.LF & "   Knowledge_Base.Include" & ASCII.LF);
      Append
        (Output,
         "     (""" & Ada.Directories.Simple_Name (Path) &
           """," & ASCII.LF);
      First_Literal := True;
      Text_IO.Open (KB_File_In, Text_IO.In_File, Path);

      while not Text_IO.End_Of_File (KB_File_In) loop
         if not First_Literal then
            Append (Output, " & ASCII.LF &" & ASCII.LF);
         else
            First_Literal := False;
         end if;
         Append (Output, "      ");
         Append (Output,
                 To_String_Literal (Text_IO.Get_Line (KB_File_In)));
      end loop;
      Append (Output, ");" & ASCII.LF);

      Text_IO.Close (KB_File_In);
   end Add_File;

   -----------------------
   -- To_String_Literal --
   -----------------------

   function To_String_Literal (S : String) return String is
      use Ada.Strings.Fixed;
      Cnt_Quotes : constant Natural := Count (S, """");
      Cnt_Tabs   : constant Natural := Count (S, "" & ASCII.HT);
      Res        : String (1 .. S'Length + Cnt_Quotes + Cnt_Tabs * 3 + 2);
      R_Pos      : Natural;

   begin
      Res (1) := '"';
      Res (Res'Last) := '"';
      R_Pos := 2;

      for C of S loop
         if C = '"' then
            Res (R_Pos .. R_Pos + 1) := """""";
            R_Pos := R_Pos + 2;

         elsif C = ASCII.HT then
            Res (R_Pos .. R_Pos + 3) := "    ";
            R_Pos := R_Pos + 4;

         else
            Res (R_Pos) := C;
            R_Pos := R_Pos + 1;
         end if;
      end loop;

      return Res;
   end To_String_Literal;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      Text_IO.Put_Line ("Usage: collect_kb [opts] dirname");
      Text_IO.Put_Line (" -h             : Show help message");
      Text_IO.Put_Line (" -o <dir>       : Output dir name");
      Text_IO.Put_Line ("                  Default is current dir");
   end Help;

   package String_Lists is new
     Ada.Containers.Indefinite_Ordered_Sets (String);
   use String_Lists;

   Search    : Directories.Search_Type;
   File      : Directories.Directory_Entry_Type;
   Entities  : Set;
   XML_Files : Set;
   Schema    : Unbounded_String;

begin
   loop
      case Command_Line.Getopt ("h o:") is
         when ASCII.NUL =>
            exit;
         when 'o' =>
            Output_File := To_Unbounded_String
              (OS_Lib.Normalize_Pathname
                (Output_Name,
                 Directory      => Command_Line.Parameter,
                 Case_Sensitive => False));
         when 'h' =>
            Help;

            return 0;
         when others =>
            raise Fail_Error
              with "collect_kb: unknown switch " & Command_Line.Full_Switch;
      end case;
   end loop;


   declare
      Opt_Kb_Dir : constant String := Command_Line.Get_Argument;
   begin
      if Opt_Kb_Dir = "" then
         Help;

         return 1;
      else
         KB_Dir := To_Unbounded_String
           (OS_Lib.Normalize_Pathname
              (Opt_Kb_Dir,
               Case_Sensitive => False));
      end if;
   end;

   if KB_Dir  = "" then
      raise Fail_Error with "collect_kb: knowledge base dirname not specified";
   elsif not OS_Lib.Is_Directory (To_String (KB_Dir)) then
      raise Fail_Error
        with "collect_kb: cannot find directory " & To_String (KB_Dir);
   end if;

   Text_IO.Put_Line ("collect_kb: parsing " & To_String (KB_Dir) & "...");

   Directories.Start_Search
    (Search,
     Directory => To_String (KB_Dir),
     Pattern   => "",
     Filter    => (Directories.Ordinary_File => True, others => False));

   while Directories.More_Entries (Search) loop
      Directories.Get_Next_Entry (Search, File);
      Text_IO.Put_Line ("  " & Directories.Full_Name (File));

      declare
         Ext : constant String :=
                 Characters.Handling.To_Lower
                  (Directories.Extension (Directories.Full_Name (File)));
      begin
         if Ext = "xml" then
            XML_Files.Include (Directories.Full_Name (File));
         elsif Ext = "ent" then
            Entities.Include (Directories.Full_Name (File));
         elsif Ext = "xsd" then
            if Schema = Null_Unbounded_String then
               Schema := To_Unbounded_String (Directories.Full_Name (File));
            else
               raise Fail_Error
                 with "collect_kb: only one schema file is allowed";
            end if;
         else
            Text_IO.Put_Line ("    unknown file type, skipping");
         end if;

      end;
   end loop;

   Directories.End_Search (Search);

   if XML_Files.Is_Empty then
      raise Fail_Error
        with "collect_kb: no xml files found in " & To_String (KB_Dir);
   end if;

   Append (Output, "package body GPR2.KB.Embedded is" & ASCII.LF);
   Append (Output, "begin" & ASCII.LF);

   if Schema /= Null_Unbounded_String then
      Add_File (To_String (Schema));
   end if;

   for Ent_File of Entities loop
      Add_File (Ent_File);
   end loop;

   Entities.Clear;

   for XML_File of XML_Files loop
      Add_File (XML_File);
   end loop;

   XML_Files.Clear;

   Append (Output, "end GPR2.KB.Embedded;" & ASCII.LF);

   declare
      type Substring is new String (1 .. Length (Output));
      package Output_IO is new Direct_IO (Substring);
      F : Output_IO.File_Type;
   begin
      Output_IO.Create (F, Output_IO.Out_File, To_String (Output_File));
      Output_IO.Write (F, Substring (To_String (Output)));
      Output_IO.Close (F);
   end;

   return 0;

exception
   when Ex : Command_Line.Invalid_Switch =>
      Text_IO.Put_Line
        (Text_IO.Standard_Error,
         "command line error: " & Ada.Exceptions.Exception_Message (Ex));
      Help;

      return 1;

   when Ex : Fail_Error =>
      Text_IO.Put_Line
        (Text_IO.Standard_Error,
         "Fatal error: " & Ada.Exceptions.Exception_Message (Ex));

      return 1;

   when Ex : others =>
      Text_IO.Put_Line
        (Text_IO.Standard_Error, Ada.Exceptions.Exception_Information (Ex));

      return 1;

end Collect_KB;
