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

with Ada.Task_Attributes;
with Ada.Characters.Handling;

with GPR2.Compilation.Registry;
with GPR2.Project.Registry.Pack;

pragma Warnings (Off);
with System.OS_Constants;
pragma Warnings (On);

with GNATCOLL.Utils;

with GPRtools.Util;

package body GPRtools.Options is

   type Object_Class is access all Object'Class;

   package TLS is new Ada.Task_Attributes (Object_Class, null);

   package PRP renames GPR2.Project.Registry.Pack;

   procedure Value_Callback (Switch, Value : String);
   --  Used for the command-line options parsing

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Object; Next : Object) is
   begin
      Self.Verbosity := Verbosity_Level'Max (Self.Verbosity, Next.Verbosity);
   end Append;

   ----------------------
   -- Clean_Build_Path --
   ----------------------

   procedure Clean_Build_Path
     (Self : in out Object; Project : GPR2.Path_Name.Object)
   is
      use GPR2, GPR2.Path_Name;

      function Project_Dir return GPR2.Path_Name.Object is
        (Create_Directory (Name_Type (Project.Dir_Name)));

   begin
      if not Self.Build_Path.Is_Defined then
         --  Check consistency of out-of-tree build options

         if Self.Root_Path.Is_Defined then
            raise Usage_Error with
              "cannot use --root-dir without --relocate-build-tree option";
         end if;

         Self.Build_Path := Project_Dir;

      elsif Self.Root_Path.Is_Defined then
         Self.Build_Path := Create_Directory
           (Project_Dir.Relative_Path (Self.Root_Path).Name,
            Name_Type (Self.Build_Path.Value));
      end if;
   end Clean_Build_Path;

   ----------------
   -- Get_Target --
   ----------------

   function Get_Target (Self : Object) return GPR2.Name_Type is
   begin
      if To_String (Self.Target) in "all" | "" then
         return GPR2.Name_Type (System.OS_Constants.Target_Name);
      else
         return GPR2.Name_Type (To_String (Self.Target));
      end if;
   end Get_Target;

   ------------------------------
   -- Read_Remaining_Arguments --
   ------------------------------

   procedure Read_Remaining_Arguments
     (Project : in out GPR2.Path_Name.Object;
      Mains   :    out GPR2.Containers.Value_Set)
   is
      use GNATCOLL;
      use GPR2.Path_Name;
   begin
      Read_Arguments : loop
         declare
            Arg : constant String := Get_Argument;
         begin
            exit Read_Arguments when Arg = "";

            if Utils.Ends_With
              ((if GPR2.File_Names_Case_Sensitive
                then Arg
                else Ada.Characters.Handling.To_Lower (Arg)),
               ".gpr")
            then
               if Project = Undefined then
                  Project := Create_File (GPR2.Name_Type (Arg));

               else
                  raise GNAT.Command_Line.Invalid_Switch with
                    '"' & Arg & """, project already """ & Project.Value & '"';
               end if;

            else
               Mains.Include (Arg);
            end if;
         end;
      end loop Read_Arguments;
   end Read_Remaining_Arguments;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Self : aliased in out Object'Class;
      Tool : Which) is
   begin
      PRP.Check_Attributes (PRP.Naming);

      case Tool is
         when Build   =>
            PRP.Check_Attributes (PRP.Builder);
            PRP.Check_Attributes (PRP.Binder);
            PRP.Check_Attributes (PRP.Linker);
            PRP.Check_Attributes (PRP.Compiler);

         when Clean  =>
            PRP.Check_Attributes (PRP.Clean);

         when Install =>
            PRP.Check_Attributes (PRP.Install);

         when Remote =>
            PRP.Check_Attributes (PRP.Remote);

         when Ls | Name => null;
      end case;

      TLS.Set_Value (Self'Unchecked_Access);

      Define_Switch
        (Self.Config, Self.Help'Access,
         "-h", Long_Switch => "--help",
         Help              => "Display this help message and exit");

      Define_Switch
        (Self.Config, Self.Version'Access,
         Long_Switch => "--version",
         Help        => "Display version and exit");

      Define_Switch
        (Self.Config, Value_Callback'Unrestricted_Access,
         "-v", "--verbose",
         Help => "Verbose output");

      if Tool /= Remote then
         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            "-q", "--quiet",
            Help => "Be quiet/terse");

         --  The code below should be uncommented when we will be able to give
         --  a name for the hiding warnings switch.
         --
         --  Define_Switch
         --    (Self.Config, Self.Warnings'Unrestricted_Access,
         --     Switch => "-ws",
         --     Help   => "Suppress all warnings",
         --     Value  => False);

         Define_Switch
           (Self.Config, Self.Full_Path_Name_For_Brief'Access,
            Switch => "-F",
            Help   => "Full project path name in brief log messages");

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--root-dir:",
            Help        => "Root directory of obj/lib/exec to relocate",
            Argument    => "<dir>");

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--relocate-build-tree?",
            Help        =>
              "Root obj/lib/exec dirs are current-directory or dir",
            Argument    => "<dir>");

         Define_Switch
           (Self.Config, Self.Debug_Mode'Access,
            Long_Switch => "--debug",
            Help        => "Debug mode");
      end if;

      if Tool in Build | Clean | Install then
         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--target=",
            Help        => "Specify a target for cross platforms",
            Argument    => "<name>");
      end if;

      if Tool in Build | Clean then
         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--implicit-with:",
            Help        =>
              "Add the given projects as a dependency on all loaded"
            & " projects",
            Argument    => "<filename>");

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--distributed?",
            Help        =>
              "Activate the distributed mode for the given slaves",
            Argument    => "<host>[,<host>]");

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--slave-env?",
            Help        =>
              "Use a specific slave's environment",
            Argument    => "<name>");
      end if;
   end Setup;

   --------------------
   -- Value_Callback --
   --------------------

   procedure Value_Callback (Switch, Value : String) is

      Self : constant not null access Object'Class := TLS.Reference.all;

      function Normalize_Value (Default : String := "") return String is
        (if Value in "" | "=" then Default
         elsif Value (Value'First) = '='
         then Value (Value'First + 1 .. Value'Last)
         else Value);
      --  Remove leading '=' symbol from value for options like
      --  --config=file.cgrp

   begin
      if Switch = "--relocate-build-tree" then
         Self.Build_Path :=
           GPR2.Path_Name.Create_Directory
             (GPR2.Name_Type (Normalize_Value (".")));

      elsif Switch = "--root-dir" then
         Self.Root_Path :=
           GPR2.Path_Name.Create_Directory
             (GPR2.Name_Type (Normalize_Value));

      elsif Switch = "-q" or else Switch = "--quiet" then
         if Self.Verbosity = Regular then
            Self.Verbosity := Quiet;
         end if;

      elsif Switch = "-v" or else Switch = "--verbose" then
         Self.Verbosity := Verbose;

      elsif Switch = "--implicit-with" then
         Self.Implicit_With.Append
           (GPR2.Path_Name.Create_File (GPR2.Name_Type (Normalize_Value)));

      elsif Switch = "--target" then
         Self.Target := To_Unbounded_String (Normalize_Value);

      elsif Switch = "--distributed" then
         declare
            use type GPR2.Containers.Count_Type;

            --  If Value is set, the first character is a =, we remove it

            Hosts : constant GPR2.Containers.Name_List :=
                      (if Value = ""
                       then GPR2.Compilation.Registry.Get_Hosts
                       else GPR2.Containers.Create
                              (GPR2.Name_Type (Normalize_Value),
                               Separator => ","));
         begin
            if Hosts.Length = 0 then
               Util.Fail_Program
                 ("missing hosts for distributed mode compilation");

            else
               GPR2.Compilation.Registry.Record_Slaves (Hosts);
               Self.Distributed_Mode := True;
            end if;
         end;

      elsif Switch = "--slave-env" then
         if Value = "" then
            Self.Slave_Env_Auto := True;
         else
            Self.Slave_Env := To_Unbounded_String (Normalize_Value);
         end if;
      end if;
   end Value_Callback;

end GPRtools.Options;
