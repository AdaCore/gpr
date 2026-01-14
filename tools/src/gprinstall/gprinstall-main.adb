------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2026, AdaCore                     --
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
with Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with GPR2.Build.Actions_Population;
with GPR2.Containers;
with GPR2.Options;
with GPR2.Project.Tree;
with GPRtools.Util;
with GPRtools.Program_Termination;

with GPRinstall.DB;
with GPRinstall.Install;
with GPRinstall.Options;
with GPRinstall.Uninstall;

function GPRinstall.Main return Ada.Command_Line.Exit_Status is

   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use GPR2;

   use GPRtools.Program_Termination;

   procedure Setup_Cross_Install (Options : in out GPRinstall.Options.Object)
     with Pre => Options.Cross_Install;
   --  Setup the cross-installation mode if target and runtime are used

   -------------------------
   -- Setup_Cross_Install --
   -------------------------

   procedure Setup_Cross_Install
     (Options : in out GPRinstall.Options.Object)
   is
      function Get_Target return String;
      --  Get target name or the empty string if we don't have a
      --  cross-compilation.

      function Get_Runtime return String;
      --  Get runtime name or "native" if we don't have a
      --  cross-compilation or using the default runtime.

      -----------------
      -- Get_Runtime --
      -----------------

      function Get_Runtime return String is
         use type Ada.Containers.Count_Type;
      begin
         --  Note that the tree runtime takes into account the command line
         --  --RTS. That is it is the final runtime for the build tree.

         if Options.Tree.Is_Defined then
            if Options.Tree.Is_Cross_Target then
               if Options.Tree.Runtime (GPR2.Ada_Language) /= "" then
                  return String (Options.Tree.Runtime (GPR2.Ada_Language));
               elsif Options.Tree.Runtime (GPR2.No_Language) /= "" then
                  return String (Options.Tree.Runtime (GPR2.No_Language));
               else
                  return "native";
               end if;

            else
               return "native";
            end if;

         else
            --  This path is when uninstalling a projet as we don't load a
            --  project in this case so we check only the command line.

            declare
               RTS : constant GPR2.Containers.Lang_Value_Map :=
                       Options.RTS_Map;
            begin
               if RTS.Length = 1 then
                  --  A single RTS, return it
                  return RTS.First_Element;

               elsif RTS.Contains (Ada_Language) then
                  --  Otherwise favor the Ada RTS
                  return RTS.Element (Ada_Language);

               elsif RTS.Contains (No_Language) then
                  --  Otherwise return the generic RTS
                  return RTS.Element (No_Language);

               else
                  --  Finally fallback (no RTS) to "native"
                  return "native";
               end if;
            end;
         end if;
      end Get_Runtime;

      ----------------
      -- Get_Target --
      ----------------

      function Get_Target return String is
         Cmd_T : constant String := String (Options.Target);
      begin
         --  Note that the tree target takes into account the command line
         --  --target. That is it is the final target for the build tree.

         if Options.Tree.Is_Defined then
            if Options.Tree.Is_Cross_Target then
               return String (Options.Tree.Target (True));
            else
               return "all";
            end if;

         else
            --  This path is when uninstalling a projet as we don't load a
            --  project in this case so we check the command line. Command
            --  line target is "all" when not specified.
            return Cmd_T;
         end if;
      end Get_Target;

      Native_Target  : constant String :=
                         String (Options.Tree.Get_KB.Normalized_Target
                                 (Project.Tree.Target_Name));
      Target         : constant String := Get_Target;
      Runtime        : constant String := Get_Runtime;

   begin
      if Target /= "" then
         declare
            Prefix : constant String :=
                       -Options.Global_Prefix_Dir.V;
            --  If Target is set, use it if it is not set and Runtime is set
            --  then use the native target name for installation.
            T_Dir  : constant String :=
                       Prefix
                         & (if Target /= "all"
                            then DS & Target
                            elsif Runtime /= "native"
                            then DS & Native_Target
                            else "")
                         & (if Runtime /= "native"
                            then DS & Runtime
                            else "");
         begin
            Options.Global_Prefix_Dir := (To_Unbounded_String (T_Dir), False);
         end;
      end if;
   end Setup_Cross_Install;

   Tree    : GPR2.Project.Tree.Object;
   Options : GPRinstall.Options.Object;

begin
   GPRtools.Util.Set_Program_Name ("gprinstall");

   --  Initialize and read the command line arguments

   GPRinstall.Options.Parse_Command_Line (Options, Tree);

   if Options.Uninstall_Mode then
      if Options.Cross_Install then
         Setup_Cross_Install (Options);
      end if;

      if Options.Global_Install_Name.Default then
         Uninstall.Process
           (String (Options.Project_File.Base_Name), Options);
      else
         Uninstall.Process
           (To_String (Options.Global_Install_Name.V), Options);
      end if;

   elsif Options.List_Mode then
      DB.List (Options);

   else
      if not Options.Load_Project (GPR2.No_Error) then
         Handle_Program_Termination (Message => "");
      end if;

      --  If verbose on, display non fatal log messages (info, warnings)

      if Options.Verbose then
         if Tree.Has_Configuration
           and then Tree.Configuration.Has_Messages
         then
            for M of Tree.Configuration.Log_Messages loop
               Tree.Reporter.Report (M.Format);
            end loop;
         end if;

         if Tree.Has_Messages then
            for M of Tree.Log_Messages.all loop
               Tree.Reporter.Report (M.Format);
            end loop;
         end if;
      end if;

      if Tree.Is_Defined
        and then Tree.Root_Project.Has_Archive_Builder
        and then Tree.Root_Project.Archive_Builder.Empty_Values
      then
         Handle_Program_Termination
           (Exit_Code => E_Success,
            Message   => "empty Archive_Builder is not supported yet.");
      end if;

      Options.Tree.Update_Sources;

      --  Initialize the actions list

      if not GPR2.Build.Actions_Population.Populate_Actions
        (Options.Tree,
         Options.Build_Options,
         Static_Actions        => True,
         With_Externally_Built => True)
      then
         raise GPRinstall_Error;
      end if;

      if Options.Cross_Install then
         Setup_Cross_Install (Options);
      end if;

      Install.Process (Options.Tree, Options);
   end if;

   return To_Exit_Status (E_Success);

exception
   when E : GPR2.Options.Usage_Error =>
      Handle_Program_Termination
        (Display_Command_Line_Help => True,
         Force_Exit                => False,
         Message                   => Exception_Message (E));
      return To_Exit_Status (E_Fatal);

   when E : GPRinstall_Error_No_Message | GPRinstall_Error =>
      Handle_Program_Termination
        (Force_Exit => False,
         Exit_Code  => E_Errors,
         Message    => Exception_Message (E));
      return To_Exit_Status (E_Errors);

   when E_Program_Termination =>
      return To_Exit_Status (E_Fatal);

   when E : others =>
      Handle_Program_Termination
        (Force_Exit => False,
         Exit_Cause => E_Generic,
         Message    => Exception_Message (E));
      return To_Exit_Status (E_Fatal);
end GPRinstall.Main;
