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

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GPR2.Interrupt_Handler;
with GPR2.KB;
with GPR2.Path_Name;
with GPR2.Project.Configuration;
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
   use GPRtools;

   Tree    : GPR2.Project.Tree.Object;

   Dummy   : aliased Boolean;
   --  A dummy boolean for supporting default switch like -a

   Config  : Project.Configuration.Object;
   Options : GPRinstall.Options.Object;

   subtype ONT is Optional_Name_Type;

   use Ada.Strings.Unbounded;

begin
   GPRtools.Util.Set_Program_Name ("gprinstall");

   --  Initialize and read the command line arguments

   GPRinstall.Options.Parse_Command_Line (Options, Tree);

   --  And install Ctrl-C handler

   Interrupt_Handler.Install_Sigint (GPRtools.Sigint.Handler'Access);

   --  Check command line arguments. These will be overridden when looking
   --  for the configuration file.
   --
   --  If configuration project is specified then load it, otherwise we will
   --  conduct an autoconf setup.

   if Options.Config_Project.Is_Defined
     and then (not Options.Create_Missing_Config
               or else Options.Config_Project.Exists)
   then
      Config := Project.Configuration.Load (Options.Config_Project);

      if Config.Has_Error then
         Util.Output_Messages (Options, Config.Log_Messages);
         GPRtools.Util.Fail_Program
           ('"' & String (Options.Config_Project.Name) &
              """ processing failed");
      end if;
   end if;

   --  Then, parse the user's project and the configuration file. Apply the
   --  configuration file to the project so that its settings are
   --  automatically inherited by the project.

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
      if Config.Is_Defined then
         Tree.Load
           (Options.Project_File, Options.Context, Config,
            Build_Path       => Options.Build_Path,
            Subdirs          => Options.Get_Subdirs,
            Src_Subdirs      => ONT (To_String (Options.Src_Subdirs)),
            Check_Shared_Lib => not Options.Unchecked_Shared_Lib);
      else
         --  No configuration, go with auto-configuration

         Tree.Load_Autoconf
           (Options.Project_File, Options.Context,
            Build_Path        => Options.Build_Path,
            Subdirs           => Options.Get_Subdirs,
            Src_Subdirs       => Options.Get_Src_Subdirs,
            Check_Shared_Lib  => not Options.Unchecked_Shared_Lib,
            Target            => Options.Get_Target,
            Language_Runtimes => Options.RTS_Map,
            Base              => GPR2.KB.Create
              (Flags      => KB.Default_Flags,
               Default_KB => not Options.Skip_Default_KB,
               Custom_KB  => Options.KB_Locations));
      end if;

      if Options.Verbose then
         for M of Tree.Log_Messages.all loop
            M.Output;
         end loop;
      end if;

      Install.Process (Tree, Options);
   end if;

exception
   when E : Usage_Error =>
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
