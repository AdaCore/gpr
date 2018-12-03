------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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
with GNATCOLL.OS.Constants;

package body GPRtools.Options is

   ------------------------------
   -- Read_Remaining_Arguments --
   ------------------------------

   procedure Read_Remaining_Arguments
     (Project : in out GPR2.Path_Name.Object;
      Mains   :    out GPR2.Containers.Value_Set)
   is
      use GNATCOLL;
      use GNATCOLL.OS;
      use GPR2.Path_Name;

      File_Name_Case_Sensitive : constant Boolean :=
                                   Constants.Default_Casing_Policy = Sensitive;
   begin
      Read_Arguments : loop
         declare
            Arg : constant String := Get_Argument;
         begin
            exit Read_Arguments when Arg = "";

            if Utils.Ends_With
              ((if File_Name_Case_Sensitive
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
   end Setup;

end GPRtools.Options;
