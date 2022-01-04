------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                      Copyright (C) 2020-2022, AdaCore                    --
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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.OS_Lib;

with Direct_IO;

procedure Collect_KB is

   use Ada;
   use Ada.Strings.Unbounded;
   use GNAT;

   Collect_KB_Error : exception;
   --  Raised to terminate execution

   Default_Output_Name : constant String := "config.kb";

   Output_File : Unbounded_String :=
                   To_Unbounded_String
                     (OS_Lib.Normalize_Pathname
                       (Default_Output_Name,
                        Directory      => Directories.Current_Directory,
                        Case_Sensitive => False));
   --  Output file where composed knoledge base should be written

   KB_Dir : Unbounded_String;
   --  Location of knowledge base

   Search  : Directories.Search_Type;
   File    : Directories.Directory_Entry_Type;

   KB_File_In : Text_IO.File_Type;

   procedure Help;
   --  Displays help on using this application

   procedure Fail (S : String);
   --  Outputs S to Standard_Error and raises Collect_KB_Error

   procedure Add_Buffer_To_Input (F_Name : String);
   --  Adds contents of Input_Buffer to All_Input, prepending it by base name
   --  of F_Name and length of buffer contents:
   --  <old_input><base_file_name>:<length>:<buffer_contents>

   ----------
   -- Fail --
   ----------

   procedure Fail (S : String) is
   begin
      Text_IO.Put_Line (Text_IO.Standard_Error, S);
      raise Collect_KB_Error;
   end Fail;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      Text_IO.Put_Line ("Usage: collect_kb [opts] dirname");
      Text_IO.Put_Line (" -h               : Show help message");
      Text_IO.Put_Line (" -v               : Verbose mode");
      Text_IO.Put_Line (" -o <file>        : Output file name");
      Text_IO.Put_Line
       ("                    Default is " & Default_Output_Name);
   end Help;

   Verbose_Mode : Boolean := False;
   --  Verbose output

   package String_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
   use String_Lists;

   Entities  : List;
   XML_Files : List;
   Schema    : Unbounded_String;

   Input_Buffer, All_Input : Unbounded_String;

   -------------------------
   -- Add_Buffer_To_Input --
   -------------------------

   procedure Add_Buffer_To_Input (F_Name : String) is
      use Ada.Strings.Fixed;
      Length_Img : constant String :=
                     Trim (Length (Input_Buffer)'Img, Ada.Strings.Both);
   begin
      Append
        (All_Input,
         Directories.Simple_Name (F_Name)
         & ":" & Length_Img & ":" & Input_Buffer);
   end Add_Buffer_To_Input;

begin
   loop
      case Command_Line.Getopt ("h v o:") is
         when ASCII.NUL =>
            exit;
            when 'o' =>
            Output_File := To_Unbounded_String
              (OS_Lib.Normalize_Pathname
                (Command_Line.Parameter,
                 Directory      => Directories.Current_Directory,
                 Case_Sensitive => False));
         when 'h' =>
            Help;
            return;
         when 'v' =>
            Verbose_Mode := True;
         when others =>
            Fail ("collect_kb: unknown switch " & Command_Line.Full_Switch);
      end case;
   end loop;

   KB_Dir := To_Unbounded_String
     (OS_Lib.Normalize_Pathname
       (Command_Line.Get_Argument,
        Case_Sensitive => False));
   if KB_Dir  = "" then
      Fail ("collect_kb: knowledge base dirname not specified");
   elsif not OS_Lib.Is_Directory (To_String (KB_Dir)) then
      Fail ("collect_kb: cannot find directory " & To_String (KB_Dir));
   end if;

   if Verbose_Mode then
      Text_IO.Put_Line ("collect_kb: parsing " & To_String (KB_Dir));
   end if;

   Directories.Start_Search
    (Search,
     Directory => To_String (KB_Dir),
     Pattern   => "",
     Filter    => (Directories.Ordinary_File => True, others => False));

   while Directories.More_Entries (Search) loop
      Directories.Get_Next_Entry (Search, File);
      if Verbose_Mode then
         Text_IO.Put_Line ("  " & Directories.Full_Name (File));
      end if;

      declare
         Ext : constant String :=
                 Characters.Handling.To_Lower
                  (Directories.Extension (Directories.Full_Name (File)));
      begin
         if Ext = "xml" then
            XML_Files.Append (Directories.Full_Name (File));
         elsif Ext = "ent" then
            Entities.Append (Directories.Full_Name (File));
         elsif Ext = "xsd" then
            if Schema = Null_Unbounded_String then
               Schema := To_Unbounded_String (Directories.Full_Name (File));
            else
               Fail ("collect_kb: only one schema file is allowed");
            end if;
         else
            if Verbose_Mode then
               Text_IO.Put_Line ("    unknown file type, skipping");
            end if;
         end if;

      end;
   end loop;

   Directories.End_Search (Search);

   if XML_Files.Is_Empty then
      Fail ("collect_kb: no xml files found in " & To_String (KB_Dir));
   end if;

   if Schema /= Null_Unbounded_String then
      Text_IO.Open (KB_File_In, Text_IO.In_File, To_String (Schema));

      while not Text_IO.End_Of_File (KB_File_In) loop
         Append (Input_Buffer, Text_IO.Get_Line (KB_File_In) & ASCII.LF);
      end loop;

      Text_IO.Close (KB_File_In);
      Add_Buffer_To_Input (To_String (Schema));
   end if;

   Input_Buffer := Null_Unbounded_String;

   for Ent_File of Entities loop
      Text_IO.Open (KB_File_In, Text_IO.In_File, Ent_File);

      while not Text_IO.End_Of_File (KB_File_In) loop
         Append (Input_Buffer, Text_IO.Get_Line (KB_File_In) & ASCII.LF);
      end loop;

      Text_IO.Close (KB_File_In);
      Add_Buffer_To_Input (Ent_File);
      Input_Buffer := Null_Unbounded_String;
   end loop;

   Entities.Clear;

   for XML_File of XML_Files loop
      Text_IO.Open (KB_File_In, Text_IO.In_File, XML_File);

      while not Text_IO.End_Of_File (KB_File_In) loop
         Append (Input_Buffer, Text_IO.Get_Line (KB_File_In) & ASCII.LF);
      end loop;

      Text_IO.Close (KB_File_In);
      Add_Buffer_To_Input (XML_File);
      Input_Buffer := Null_Unbounded_String;
   end loop;

   XML_Files.Clear;

   declare
      type Substring is new String (1 .. Length (All_Input));
      package Output is new Direct_IO (Substring);
      F : Output.File_Type;
   begin
      Output.Create (F, Output.Out_File, To_String (Output_File));
      Output.Write (F, Substring (To_String (All_Input)));
      Output.Close (F);
   end;

exception
   when Ex : Command_Line.Invalid_Switch =>
      Text_IO.Put_Line
        (Text_IO.Standard_Error, Ada.Exceptions.Exception_Message (Ex));
      Help;
      OS_Lib.OS_Exit (1);
   when Ex : others =>
      Text_IO.Put_Line
        (Text_IO.Standard_Error, Ada.Exceptions.Exception_Information (Ex));
      OS_Lib.OS_Exit (2);
end Collect_KB;
