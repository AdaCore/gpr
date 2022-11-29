------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GPR2.Interrupt_Handler;
with GPR2.Options;
with GPR2.Project.Tree;
with GPRtools.Command_Line;
with GPRtools.Sigint;
with GPRtools.Util;

with GPRinstall.DB;
with GPRinstall.Install;
with GPRinstall.Options;
with GPRinstall.Uninstall;

procedure GPRinstall.Main is

   use Ada;
   use Ada.Exceptions;

   use GPR2;

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
      if not Options.Load_Project (Tree  => Options.Tree.all,
                                   Quiet => Options.Quiet)
      then
         GPRtools.Util.Project_Processing_Failed (Options);
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

      Install.Process (Tree, Options);
   end if;

exception
   when E : GPR2.Options.Usage_Error =>
      Ada.Text_IO.Put_Line ("gprinstall: " & Exception_Message (E));
      Ada.Text_IO.Flush;
      GPRtools.Command_Line.Try_Help;
      GPRtools.Util.Exit_Program (GPRtools.Util.E_Fatal);

   when Project_Error | Processing_Error =>
      GPRtools.Util.Project_Processing_Failed (Options);

   when E : GPRinstall_Error_No_Message =>
      if Options.Verbose then
         Text_IO.Put_Line
           ("gprinstall: " & Exception_Information (E));
      end if;
      Text_IO.Flush;
      GPRtools.Util.Exit_Program (GPRtools.Util.E_Errors);

   when E : GPRinstall_Error =>
      Text_IO.Put_Line
        ("gprinstall: "
         & (if Options.Verbose
            then Exception_Information (E)
            else Exception_Message (E)));
      Text_IO.Flush;
      GPRtools.Util.Exit_Program (GPRtools.Util.E_Errors);

   when E : others =>
      Text_IO.Put_Line ("error: " & Exception_Information (E));
      Text_IO.Flush;
      GPRtools.Util.Exit_Program (GPRtools.Util.E_Fatal);
end GPRinstall.Main;
