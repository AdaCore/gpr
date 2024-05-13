------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
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
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with GPR2.Interrupt_Handler;
with GPR2.Options;
with GPR2.Project.Tree;
with GPRtools.Sigint;
with GPRtools.Util;
with GPRtools.Program_Termination;

with GPRinstall.DB;
with GPRinstall.Install;
with GPRinstall.Options;
with GPRinstall.Uninstall;

function GPRinstall.Main return Ada.Command_Line.Exit_Status is

   use Ada;
   use Ada.Exceptions;

   use GPR2;

   use GPRtools.Program_Termination;

   Tree    : GPR2.Project.Tree.Object;

   Dummy   : aliased Boolean;
   --  A dummy boolean for supporting default switch like -a

   Options : GPRinstall.Options.Object;

   use Ada.Strings.Unbounded;

begin
   GPRtools.Util.Set_Program_Name ("gprinstall");

   --  Initialize and read the command line arguments

   GPRinstall.Options.Parse_Command_Line (Options, Tree);

   --  And install Ctrl-C handler

   Interrupt_Handler.Install_Sigint (GPRtools.Sigint.Handler'Access);

   if Options.Uninstall_Mode then
      if Options.Global_Install_Name.Default then
         Uninstall.Process (Options.Args.First_Element, Options);
      else
         Uninstall.Process
           (To_String (Options.Global_Install_Name.V), Options);
      end if;

   elsif Options.List_Mode then
      DB.List (Options);

   else
      if not Options.Load_Project
        (Absent_Dir_Error   => Project.Tree.No_Error,
         Handle_Information => Options.Verbose,
         Handle_Lint        => Options.Verbose)
      then
         Handle_Program_Termination
           (Opt     => Options,
            Message => '"'
            & (if Options.Config_Project_Has_Error
              then String (Options.Config_Project.Simple_Name)
              else String (Options.Filename.Simple_Name))
            & """ processing failed");
      end if;

      if Options.Verbose then
         for M of Options.Config_Project_Log loop
            M.Output;
         end loop;

         if Tree.Has_Messages then
            for M of Tree.Log_Messages.all loop
               M.Output;
            end loop;
         end if;
      end if;

      if Tree.Is_Defined
        and then Tree.Root_Project.Has_Archive_Builder
        and then Tree.Root_Project.Archive_Builder.Empty_Values
      then
         Handle_Program_Termination
           (Opt       => Options,
            Exit_Code => E_Success,
            Message   => "empty Archive_builder is not supported yet.");
      end if;

      Install.Process (Tree, Options);
   end if;

   return To_Exit_Status (E_Success);

exception
   when E : GPR2.Options.Usage_Error =>
      Handle_Program_Termination
        (Opt                       => Options,
         Display_Command_Line_Help => True,
         Force_Exit                => False,
         Message                   => Exception_Message (E));
      return To_Exit_Status (E_Fatal);

   when Project_Error | Processing_Error =>
      Handle_Program_Termination
        (Opt                   => Options,
         Display_Tree_Messages => True,
         Force_Exit            => False,
         Message               => '"'
         & (if Options.Config_Project_Has_Error
            then String (Options.Config_Project.Simple_Name)
            else String (Options.Filename.Simple_Name))
         & """ processing failed");
      return To_Exit_Status (E_Fatal);

   when E : GPRinstall_Error_No_Message | GPRinstall_Error =>
      Handle_Program_Termination
        (Opt        => Options,
         Force_Exit => False,
         Exit_Code  => E_Errors,
         Message    => Exception_Message (E));
      return To_Exit_Status (E_Errors);

   when E_Program_Termination =>
      return To_Exit_Status (E_Fatal);

   when E : others =>
      Handle_Program_Termination
        (Opt        => Options,
         Force_Exit => False,
         Exit_Cause => E_Generic,
         Message    => Exception_Message (E));
      return To_Exit_Status (E_Fatal);
end GPRinstall.Main;
