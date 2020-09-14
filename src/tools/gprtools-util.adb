------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Case_Util;
with GNAT.OS_Lib;
with GNATCOLL.Utils;

with GPR2.Containers;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Tree;

package body GPRtools.Util is

   use Ada;
   use Ada.Strings.Unbounded;

   Partial_Prefix : constant Name_Type := "p__";

   Keep_Program_Name : Unbounded_String;

   procedure Exit_Program (Exit_Code : Exit_Code_Type);
   pragma No_Return (Exit_Program);
   --  A call to Exit_Program terminates execution with the given status.
   --  A status of zero indicates normal completion, a non-zero status
   --  indicates abnormal termination.

   -------------------------------
   -- Check_For_Default_Project --
   -------------------------------

   procedure Check_For_Default_Project
     (Options : in out GPRtools.Options.Object'Class)
   is
      use Directories;
      Default_Name : constant String := "default.gpr";
      Search       : Search_Type;
      Item         : Directory_Entry_Type;

   begin
      if Exists (Default_Name)
        and then Kind (Default_Name) = Ordinary_File
      then
         Options.Project_File :=
           Path_Name.Create_File (Name_Type (Default_Name));
         return;
      end if;

      Start_Search
        (Search, ".", "*.gpr", (Ordinary_File => True, others => False));

      if More_Entries (Search) then
         Get_Next_Entry (Search, Item);

         if not More_Entries (Search) then
            --  Only one project in current directory can be default one

            Options.Project_File :=
              Path_Name.Create_File (Name_Type (Full_Name (Item)));

            if not Options.Quiet then
               Text_IO.Put_Line
                 ("using project file " & Options.Project_File.Value);
            end if;
         end if;

      else
         Options.Project_File := Path_Name.Implicit_Project;
         Options.Project_Base :=
           Path_Name.Create_Directory (Name_Type (Current_Directory));

         if not Options.Quiet then
            Text_IO.Put_Line
              ("use implicit project in " & Options.Project_Base.Value);
         end if;
      end if;
   end Check_For_Default_Project;

   ----------------------------
   -- Executable_Prefix_Path --
   ----------------------------

   function Executable_Prefix_Path return String is
      use GNAT;
      use GNATCOLL;

      Exec_Name : constant String := Command_Line.Command_Name;

      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceded by the absolute or relative path,
      --  e.g. "c:\usr\bin\gcc.exe". Returns the absolute directory where "bin"
      --  lies (in the example "C:\usr"). If the executable is not in a "bin"
      --  directory, return "".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         Exec      : String  :=
                       OS_Lib.Normalize_Pathname (S, Resolve_Links => True);
         Path_Last : Integer := 0;

      begin
         for J in reverse Exec'Range loop
            if Utils.Is_Directory_Separator (Exec (J)) then
               Path_Last := J - 1;
               exit;
            end if;
         end loop;

         if Path_Last >= Exec'First + 2 then
            Case_Util.To_Lower (Exec (Path_Last - 2 .. Path_Last));
         end if;

         if Path_Last < Exec'First + 2
           or else Exec (Path_Last - 2 .. Path_Last) /= "bin"
           or else (Path_Last - 3 >= Exec'First
                      and then
                    not Utils.Is_Directory_Separator (Exec (Path_Last - 3)))
         then
            return "";
         end if;

         return (Exec (Exec'First .. Path_Last - 4))
           & OS_Lib.Directory_Separator;
      end Get_Install_Dir;

   --  Beginning of Executable_Prefix_Path

   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      if GPR2.Has_Directory_Separator (Exec_Name) then
         return Get_Install_Dir (Exec_Name);
      end if;

      --  If we get here, the user has typed the executable name with no
      --  directory prefix.

      declare
         use type OS_Lib.String_Access;
         Path : OS_Lib.String_Access := OS_Lib.Locate_Exec_On_Path (Exec_Name);
      begin
         if Path = null then
            return "";
         else
            declare
               Dir : constant String := Get_Install_Dir (Path.all);
            begin
               OS_Lib.Free (Path);
               return Dir;
            end;
         end if;
      end;
   end Executable_Prefix_Path;

   ------------------
   -- Exit_Program --
   ------------------

   procedure Exit_Program (Exit_Code : Exit_Code_Type) is
      use GNAT.OS_Lib;
   begin
      --  The program will exit with the following status:

      --    0 if the object file has been generated (with or without warnings)
      --    1 if recompilation was not needed (smart recompilation)
      --    4 for a fatal error
      --    5 if there were errors
      --    6 if no code has been generated (spec)

      --  Note that exit code 3 is not used and must not be used as this is
      --  the code returned by a program aborted via C abort() routine on
      --  Windows. GCC checks for that case and thinks that the child process
      --  has been aborted. This code (exit code 3) used to be the code used
      --  for E_No_Code, but E_No_Code was changed to 6 for this reason.

      case Exit_Code is
         when E_Success    => OS_Exit (0);
         when E_Warnings   => OS_Exit (0);
         when E_No_Compile => OS_Exit (1);
         when E_Fatal      => OS_Exit (4);
         when E_Errors     => OS_Exit (5);
         when E_No_Code    => OS_Exit (6);
         when E_Abort      => OS_Abort;
      end case;
   end Exit_Program;

   ------------------
   -- Fail_Program --
   ------------------

   procedure Fail_Program (Message : String) is
   begin
      Finish_Program (E_Fatal, Message);
   end Fail_Program;

   --------------------
   -- Finish_Program --
   --------------------

   procedure Finish_Program
     (Exit_Code : Exit_Code_Type := E_Success;
      Message   : String := "")
   is
      use Ada.Text_IO;
   begin
      if Message'Length > 0 then
         Put_Line
           ((if Exit_Code = E_Success
             then Standard_Output
             else Standard_Error),
            To_String (Keep_Program_Name) & ": " & Message);
      end if;

      Exit_Program (Exit_Code);
   end Finish_Program;

   ----------------------------
   -- Is_Ada_Predefined_Unit --
   ----------------------------

   function Is_Ada_Predefined_Unit (Unit : Name_Type) return Boolean is
      use GNATCOLL.Utils;

      Lower_Unit : constant String := To_Lower (Unit);
   begin
      return Lower_Unit = "ada"
        or else Lower_Unit = "gnat"
        or else Lower_Unit = "interfaces"
        or else Lower_Unit = "system"
        or else Lower_Unit = "calendar"
        or else Lower_Unit = "machine_code"
        or else Lower_Unit = "unchecked_conversion"
        or else Lower_Unit = "unchecked_deallocation"
        or else Lower_Unit = "direct_io"
        or else Lower_Unit = "io_exceptions"
        or else Lower_Unit = "sequential_io"
        or else Lower_Unit = "text_io"
        or else Starts_With (Lower_Unit, "ada.")
        or else Starts_With (Lower_Unit, "gnat.")
        or else Starts_With (Lower_Unit, "system.")
        or else Starts_With (Lower_Unit, "interfaces.");
   end Is_Ada_Predefined_Unit;

   ---------------------
   -- Output_Messages --
   ---------------------

   procedure Output_Messages
     (Options : GPRtools.Options.Object'Class;
      Log     : GPR2.Log.Object := GPR2.Log.Undefined)
   is
      use Ada.Text_IO;
      use GPR2.Log;
      Displayed : GPR2.Containers.Value_Set;
      Used_Log  : constant GPR2.Log.Object :=
                    (if not Log.Is_Defined and then Options.Tree /= null
                       and then Options.Tree.Has_Messages
                     then Options.Tree.Log_Messages.all
                     else Log);
   begin
      for C in Used_Log.Iterate
        (Information => Options.Verbosity = Verbose,
         Warning     => Options.Warnings,
         Error       => True,
         Read        => True,
         Unread      => True)
      loop
         declare
            use GPR2.Message;

            Msg      : constant GPR2.Log.Constant_Reference_Type :=
                         Used_Log.Constant_Reference (C);
            Text     : constant String :=
                         Msg.Format (Options.Full_Path_Name_For_Brief);
            Dummy    : GPR2.Containers.Value_Type_Set.Cursor;
            Inserted : Boolean;
         begin
            Displayed.Insert (Text, Dummy, Inserted);

            if Inserted then
               Put_Line
                 (File_Access'
                    (case Msg.Level is
                        when Information     => Current_Output,
                        when Warning | Error => Current_Error).all,
                  Text);
            end if;
         end;
      end loop;
   end Output_Messages;

   ------------------
   -- Partial_Name --
   ------------------

   function Partial_Name
     (Lib_Name      : Name_Type;
      Number        : Natural;
      Object_Suffix : Name_Type) return Name_Type
   is
      Img : constant String := Number'Img;
   begin
      return Name_Type (String (Partial_Prefix) & String (Lib_Name)
                        & '_' & Img (Img'First + 1 .. Img'Last)
                        & String (Object_Suffix));
   end Partial_Name;

   -------------------------------
   -- Project_Processing_Failed --
   -------------------------------

   procedure Project_Processing_Failed
     (Options : GPRtools.Options.Object'Class) is
   begin
      Output_Messages (Options);
      Fail_Program
        ('"'
         & (if Options.Project_File.Is_Defined
            then String (Options.Project_File.Simple_Name)
            else Options.Project_Base.Value)
         & """ processing failed");
   end Project_Processing_Failed;

   ----------------------
   -- Set_Program_Name --
   ----------------------

   procedure Set_Program_Name (Name : String) is
      use Ada.Environment_Variables;
      GPR_Tool : constant String := "GPR_TOOL";
   begin
      Keep_Program_Name := To_Unbounded_String (Name);

      if Value (GPR_Tool, Default => "") = "" then
         Set
           (GPR_Tool,
            (if Name in "gprclean" | "gprbuild" | "gprls" | "gprname"
                 | "gprinstall" | "gprdump"
             then "gprbuild"
             else Name));
      end if;
   end Set_Program_Name;

end GPRtools.Util;
