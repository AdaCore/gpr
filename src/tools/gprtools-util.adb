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

with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;

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
