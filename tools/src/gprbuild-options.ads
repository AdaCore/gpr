------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Ada.Containers.Ordered_Maps;

with GPR2;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Unit;

with GPRtools.Options;

package GPRbuild.Options is

   package Lang_Args is new Ada.Containers.Ordered_Maps
     (GPR2.Language_Id, GPR2.Containers.Value_List,
      GPR2."<",
      GPR2.Containers.Value_Type_List."=");

   type Compilation_Mode is (Normal, Minimal, Checksum);

   type Object is new GPRtools.Options.Base_Options with record
      Single_Build_Per_Obj_Dir : Boolean := False;
      Build_Script             : GPR2.Path_Name.Object;
      Indirect_Imports         : Boolean := True;
      No_Object_Check          : Boolean := False;
      No_SAL_Binding           : Boolean := False;
      No_Run_Path              : Boolean := False;
      Restricted_To_Languages  : GPR2.Containers.Language_Set;
      Display_Progress         : Boolean := False;

      Build_If_Switch_Changes  : Boolean := False;
      Force                    : Boolean := False;
      Keep_Going               : Boolean := False;
      Mode                     : Compilation_Mode := Normal;
      Multi_Unit_Index         : GPR2.Unit_Index := GPR2.No_Index;
      Output_File              : GPR2.Path_Name.Object;
      Create_Missing_Dirs      : Boolean := False;
      Force_Recursive_Build    : Boolean := False;

      Restricted_Build_Phase   : Boolean := False;
      Bind_Phase_Mandated      : Boolean := False;
      Compile_Phase_Mandated   : Boolean := False;
      Link_Phase_Mandated      : Boolean := False;
      Unique_Recompilation     : Boolean := False;

      Parallel_Compilation     : Natural := 1;
      Parallel_Bind            : Natural := 1;
      Parallel_Link            : Natural := 1;

      Compiler_Args            : Lang_Args.Map;
      Binder_Args              : Lang_Args.Map;
      Linker_Args              : GPR2.Containers.Value_List;
      Config_Args              : GPR2.Containers.Value_List;
   end record;
   --  Options for gprls

   type GPRBuild_Parser is
     new GPRtools.Options.Command_Line_Parser with null record;

   function Create return GPRBuild_Parser;

   function Mains
     (Options : Object) return GPR2.Unit.Source_Unit_Vectors.Vector;
   --  The list of main units to compile

   function Recursive_Build (Options : Object) return Boolean is
     (not Options.Unique_Recompilation
      or else Options.Force_Recursive_Build);
   --  Whether we should build the whole tree (except of course Extrnally_Built
   --  projects.

   function Do_Compilation (Options : Object) return Boolean is
     (not Options.Restricted_Build_Phase
      or else Options.Compile_Phase_Mandated);
   --  Whether the compilation phase is to be considered

   function Do_Binding (Options : Object) return Boolean is
     (not Options.Unique_Recompilation
      and then (not Options.Restricted_Build_Phase
        or else Options.Bind_Phase_Mandated));
   --  Whether the bindibg phase is to be considered

   function Do_Link (Options : Object) return Boolean is
     (not Options.Unique_Recompilation
      and then (not Options.Restricted_Build_Phase
        or else Options.Link_Phase_Mandated));
   --  Whether the linking phase is to be considered

end GPRbuild.Options;
