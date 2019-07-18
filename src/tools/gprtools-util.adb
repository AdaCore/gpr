------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Case_Util;
with GNAT.OS_Lib;

with GPR2.Containers;
with GPR2.Message;

package body GPRtools.Util is

   use Ada;
   use Ada.Strings.Unbounded;

   use GPR2;

   Keep_Program_Name : Unbounded_String;

   procedure Exit_Program (Exit_Code : Exit_Code_Type);
   pragma No_Return (Exit_Program);
   --  A call to Exit_Program terminates execution with the given status.
   --  A status of zero indicates normal completion, a non-zero status
   --  indicates abnormal termination.

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

   ------------------------------
   -- Look_For_Default_Project --
   ------------------------------

   function Look_For_Default_Project
     (Quiet : Boolean) return GPR2.Path_Name.Object
   is

      function Executable_Prefix_Path return String;

      ----------------------------
      -- Executable_Prefix_Path --
      ----------------------------

      function Executable_Prefix_Path return String is
         use GNAT.OS_Lib;

         --  Exec_Name : constant String := Ada.Command_Line.Command_Name;
         Exec_Name : constant String := "gnat";

         function Get_Install_Dir (S : String) return String;
         --  S is the executable name preceded by the absolute or relative
         --  path, e.g. "c:\usr\bin\gcc.exe". Returns the absolute directory
         --  where "bin" lies (in the example "C:\usr").
         --  If the executable is not in a "bin" directory, returns "".

         ---------------------
         -- Get_Install_Dir --
         ---------------------

         function Get_Install_Dir (S : String) return String is
            use GNAT.Case_Util;

            Exec      : String  :=
                          Normalize_Pathname (S, Resolve_Links => True);
            Path_Last : Integer := 0;

         begin
            for J in reverse Exec'Range loop
               if Exec (J) = Directory_Separator then
                  Path_Last := J - 1;
                  exit;
               end if;
            end loop;

            if Path_Last >= Exec'First + 2 then
               To_Lower (Exec (Path_Last - 2 .. Path_Last));
            end if;

            if Path_Last < Exec'First + 2
              or else Exec (Path_Last - 2 .. Path_Last) /= "bin"
              or else (Path_Last - 3 >= Exec'First
                       and then Exec (Path_Last - 3) /= Directory_Separator)
            then
               return "";
            end if;

            return (Exec (Exec'First .. Path_Last - 4)) & Directory_Separator;
         end Get_Install_Dir;

      begin
         --  First determine if a path prefix was placed in front of the
         --  executable name.

         for J in reverse Exec_Name'Range loop
            if Exec_Name (J) = Directory_Separator then
               return Get_Install_Dir (Exec_Name);
            end if;
         end loop;

         --  If we get here, the user has typed the executable name with no
         --  directory prefix.

         declare
            Path : GNAT.OS_Lib.String_Access :=
                     Locate_Exec_On_Path (Exec_Name);
         begin
            if Path = null then
               return "";
            else
               declare
                  Dir : constant String := Get_Install_Dir (Path.all);
               begin
                  Free (Path);
                  return Dir;
               end;
            end if;
         end;
      end Executable_Prefix_Path;

      Result : GPR2.Path_Name.Object;

   begin
      Result := GPR2.Project.Look_For_Default_Project;

      if not Result.Is_Defined then
         Result := Path_Name.Create_File
           (Optional_Name_Type
              (Executable_Prefix_Path & "/share/gpr/_default.gpr"));

         if not Result.Exists then
            return Path_Name.Undefined;
         end if;
      end if;

      if not Quiet then
         Text_IO.Put_Line ("using project file " & Result.Value);
      end if;

      return Result;
   end Look_For_Default_Project;

   ---------------------
   -- Output_Messages --
   ---------------------

   procedure Output_Messages
     (Log : GPR2.Log.Object; Options : GPRtools.Options.Object'Class)
   is
      use Ada.Text_IO;
      Displayed : GPR2.Containers.Value_Set;
   begin
      for C in Log.Iterate
        (Information => Options.Verbosity = Verbose,
         Warning     => Options.Warnings,
         Error       => True,
         Read        => True,
         Unread      => True)
      loop
         declare
            use GPR2.Message;

            Msg      : constant GPR2.Log.Constant_Reference_Type :=
                         Log.Constant_Reference (C);
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

   -------------------------------
   -- Project_Processing_Failed --
   -------------------------------

   procedure Project_Processing_Failed
     (Tree    : GPR2.Project.Tree.Object;
      Options : GPRtools.Options.Object'Class) is
   begin
      Output_Messages (Tree.Log_Messages.all, Options);
      Fail_Program
        ('"' & String (Tree.Root_Project.Path_Name.Simple_Name)
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
