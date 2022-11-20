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

with Ada.Strings.Unbounded;

with GPR2;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;

with GPRtools.Options;

package GPRls.Options is

   use Ada.Strings.Unbounded;

   use GPR2;

   type Object is new GPRtools.Options.Base_Options with private;
   --  Options for gprls

   function Build_From_Command_Line (Self : in out Object) return Boolean;
   --  Fill out a gprls options object from the command line

   function Files (Self : Object) return Containers.Value_Set;

   function Project_Context (Self : Object) return Context.Object;

   function List_File (Self : Object) return Path_Name.Object;

   function With_Predefined_Units (Self : Object) return Boolean;

   function Hide_Runtime_Directory (Self : Object) return Boolean;

   function Print_Units (Self : Object) return Boolean;

   function Print_Sources (Self : Object) return Boolean;

   function Print_Object_Files (Self : Object) return Boolean;

   function Source_Parser (Self : Object) return Boolean;

   function Dependency_Mode (Self : Object) return Boolean;

   function Closure_Mode (Self : Object) return Boolean;

   function All_Projects (Self : Object) return Boolean;

   function Selective_Output (Self : Object) return Boolean;

   function Only_Display_Paths (Self : Object) return Boolean;

   function Gnatdist (Self : Object) return Boolean;

   procedure Print (Self : Object);

private

   type Output_Type is (Units, Sources, Objects);
   type Selective_Output_Type is array (Output_Type) of Boolean;

   --  All_Outputs is the default, and corresponds to all values set to False
   All_Outputs : constant Selective_Output_Type := (others => False);

   type Object is new GPRtools.Options.Base_Options with record
      List_File             : Path_Name.Object;
      With_Predefined_Units : Boolean := False;
      Hide_Predefined_Path  : Boolean := False;
      Selective_Output      : Selective_Output_Type := All_Outputs;
      Dependency_Mode       : Boolean := False;
      Closure_Mode          : Boolean := False;
      All_Projects          : Boolean := False;
      Only_Display_Paths    : Boolean := False;
      Source_Parser         : Boolean := False;
      Gnatdist              : Boolean := False;
   end record;

   function Files (Self : Object) return GPR2.Containers.Value_Set is
     (Self.Args);

   function Only_Display_Paths (Self : Object) return Boolean is
     (Self.Only_Display_Paths);

   function Project_Context (Self : Object) return Context.Object is
     (Self.Context);

   function List_File (Self : Object) return Path_Name.Object is
     (Self.List_File);

   function With_Predefined_Units (Self : Object) return Boolean is
     (Self.With_Predefined_Units);

   function Hide_Runtime_Directory (Self : Object) return Boolean is
     (Self.Hide_Predefined_Path);

   function Print_Units (Self : Object) return Boolean is
     (Self.Selective_Output = All_Outputs
      or else Self.Selective_Output (Units));

   function Print_Sources (Self : Object) return Boolean is
     (Self.Selective_Output = All_Outputs
      or else Self.Selective_Output (Sources));

   function Print_Object_Files (Self : Object) return Boolean is
     (Self.Selective_Output = All_Outputs
      or else Self.Selective_Output (Objects));

   function Source_Parser (Self : Object) return Boolean is
     (Self.Source_Parser);

   function Dependency_Mode (Self : Object) return Boolean is
     (Self.Dependency_Mode);

   function Closure_Mode (Self : Object) return Boolean is
     (Self.Closure_Mode);

   function All_Projects (Self : Object) return Boolean is
     (Self.All_Projects);

   function Selective_Output (Self : Object) return Boolean is
     (Self.Selective_Output /= All_Outputs);

   function Gnatdist (Self : Object) return Boolean is
     (Self.Gnatdist);

end GPRls.Options;
