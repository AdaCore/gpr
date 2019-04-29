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

with Ada.Characters.Handling;

with GNATCOLL.Utils;

package body GPRtools.Options is

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

   procedure Setup (Self : in out Object) is

      procedure Value_Callback (Switch, Value : String);

      --------------------
      -- Value_Callback --
      --------------------

      procedure Value_Callback (Switch, Value : String) is

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
         end if;
      end Value_Callback;

   begin
      Define_Switch
        (Self.Config, Self.Help'Access,
         "-h", Long_Switch => "--help",
         Help              => "Display this help message and exit");

      Define_Switch
        (Self.Config, Self.Version'Access,
         Long_Switch => "--version",
         Help        => "Display version and exit");

      Define_Switch
        (Self.Config, Self.Verbose'Access,
         "-v", "--verbose",
         Help => "Verbose output");

      Define_Switch
        (Self.Config, Self.Quiet'Access,
         "-q", "--quiet",
         Help => "Be quiet/terse");

      Define_Switch
        (Self.Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--root-dir:",
         Help        => "Root directory of obj/lib/exec to relocate",
         Argument    => "<dir>");

      Define_Switch
        (Self.Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--relocate-build-tree?",
         Help        => "Root obj/lib/exec dirs are current-directory or dir",
         Argument    => "<dir>");
   end Setup;

end GPRtools.Options;
