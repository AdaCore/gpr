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

with Ada.Strings.Unbounded;

with GPR2;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;

with GPRls.Common;

package GPRls.Options is

   use Ada.Strings.Unbounded;

   use GPR2;

   use GPRls.Common;

   Usage_Error : exception;
   --  Raised when a wrong usage is detected

   type Object is tagged private;
   --  Options for gprls

   procedure Build_From_Command_Line (Self : in out Object);
   --  Fill out a gprls options object from the command line

   function Files (Self : Object) return String_Vector.Vector;

   function Verbosity (Self : Object) return Verbosity_Level_Type;

   function Usage_Needed (Self : Object) return Boolean;

   function Version_Needed (Self : Object) return Boolean;

   function Project_File (Self : Object) return Path_Name.Object;

   function Project_Context (Self : Object) return Context.Object;

   function Project_Search_Paths (Self : Object) return Path_Name.Set.Object;

   function RTS (Self : Object) return String;

   function Target (Self : Object) return String;

   function List_File (Self : Object) return Path_Name.Object;

   function With_Predefined_Units (Self : Object) return Boolean;

   function Print_Units (Self : Object) return Boolean;

   function Print_Sources (Self : Object) return Boolean;

   function Print_Object_Files (Self : Object) return Boolean;

   function Dependency_Mode (Self : Object) return Boolean;

   function Closure_Mode (Self : Object) return Boolean;

   function All_Projects (Self : Object) return Boolean;

   function Selective_Output (Self : Object) return Boolean;

   function Only_Display_Paths (Self : Object) return Boolean;

   procedure Print (Self : Object);

private

   type Object is tagged record
      Files                : String_Vector.Vector;
      Project_File         : Path_Name.Object     := Path_Name.Undefined;
      Project_Search_Paths : Path_Name.Set.Object :=
                               Path_Name.Set.Set.Empty_List;
      Target               : Unbounded_String     := Null_Unbounded_String;
      RTS                  : Unbounded_String     := Null_Unbounded_String;
      List_File            : Path_Name.Object     := Path_Name.Undefined;
      Project_Context      : Context.Object       := GPR2.Context.Empty;

      With_Predefined_Units : Boolean := False;
      Print_Units           : Boolean := True;
      Print_Sources         : Boolean := True;
      Print_Object_Files    : Boolean := True;
      Selective_Output      : Boolean := False;
      Dependency_Mode       : Boolean := False;
      Closure_Mode          : Boolean := False;
      All_Projects          : Boolean := False;

      Verbosity       : Verbosity_Level_Type := None;
      Verbose_Parsing : Integer              := 0;

      Usage_Needed   : Boolean := False;
      Version_Needed : Boolean := False;

      Only_Display_Paths : Boolean := False;
   end record;

   function Files (Self : Object) return String_Vector.Vector is
     (Self.Files);

   function Verbosity (Self : Object) return Verbosity_Level_Type is
     (Self.Verbosity);

   function Usage_Needed (Self : Object) return Boolean is
     (Self.Usage_Needed);

   function Version_Needed (Self : Object) return Boolean is
     (Self.Version_Needed);

   function Only_Display_Paths (Self : Object) return Boolean is
     (Self.Only_Display_Paths);

   function Project_Context (Self : Object) return Context.Object is
     (Self.Project_Context);

   function Project_File (Self : Object) return Path_Name.Object is
     (Self.Project_File);

   function Project_Search_Paths (Self : Object) return Path_Name.Set.Object is
     (Self.Project_Search_Paths);

   function RTS (Self : Object) return String is
     (To_String (Self.RTS));

   function Target (Self : Object) return String is
     (To_String (Self.Target));

   function List_File (Self : Object) return Path_Name.Object is
     (Self.List_File);

   function With_Predefined_Units (Self : Object) return Boolean is
     (Self.With_Predefined_Units);

   function Print_Units (Self : Object) return Boolean is
     (Self.Print_Units);

   function Print_Sources (Self : Object) return Boolean is
     (Self.Print_Sources);

   function Print_Object_Files (Self : Object) return Boolean is
     (Self.Print_Object_Files);

   function Dependency_Mode (Self : Object) return Boolean is
     (Self.Dependency_Mode);

   function Closure_Mode (Self : Object) return Boolean is
     (Self.Closure_Mode);

   function All_Projects (Self : Object) return Boolean is
     (Self.All_Projects);

   function Selective_Output (Self : Object) return Boolean is
     (Self.Selective_Output);

end GPRls.Options;
