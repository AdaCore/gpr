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
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;

with GNAT.Case_Util;
with GNAT.OS_Lib;
with GNATCOLL.Utils;

package body GPRtools.Util is

   use Ada;
   use Ada.Strings.Unbounded;

   Partial_Prefix : constant Simple_Name := "p__";

   Keep_Program_Name : Unbounded_String;

   ----------------------------
   -- Executable_Prefix_Path --
   ----------------------------

   function Executable_Prefix_Path return String is
      use GNAT;
      use GNATCOLL;

      Exec_Name : constant String := Ada.Command_Line.Command_Name;

      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceded by the absolute or relative path,
      --  e.g. "c:\usr\bin\gcc.exe". Returns the absolute directory where "bin"
      --  lies (in the example "C:\usr"). If the executable is not in a "bin"
      --  directory, return "".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         Exec      : String  :=
                       OS_Lib.Normalize_Pathname (S, Resolve_Links => True);
         Path_Last : Integer := 0;

      begin
         for J in reverse Exec'Range loop
            if Utils.Is_Directory_Separator (Exec (J)) then
               Path_Last := J - 1;
               exit;
            end if;
         end loop;

         if Path_Last >= Exec'First + 2 then
            Case_Util.To_Lower (Exec (Path_Last - 2 .. Path_Last));
         end if;

         if Path_Last < Exec'First + 2
           or else Exec (Path_Last - 2 .. Path_Last) /= "bin"
           or else (Path_Last - 3 >= Exec'First
                      and then
                    not Utils.Is_Directory_Separator (Exec (Path_Last - 3)))
         then
            return "";
         end if;

         return (Exec (Exec'First .. Path_Last - 4))
           & OS_Lib.Directory_Separator;
      end Get_Install_Dir;

   --  Beginning of Executable_Prefix_Path

   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      if GPR2.Has_Directory_Separator (Exec_Name) then
         return Get_Install_Dir (Exec_Name);
      end if;

      --  If we get here, the user has typed the executable name with no
      --  directory prefix.

      declare
         Path : constant String := Locate_Exec_On_Path (Exec_Name);
      begin
         return (if Path = "" then "" else Get_Install_Dir (Path));
      end;
   end Executable_Prefix_Path;

   ----------------------
   -- Get_Program_Name --
   ----------------------

   function Get_Program_Name return String is (To_String (Keep_Program_Name));

   ----------------------------
   -- Is_Ada_Predefined_Unit --
   ----------------------------

   function Is_Ada_Predefined_Unit (Unit : Name_Type) return Boolean is
      use GNATCOLL.Utils;

      Lower_Unit : constant String := To_Lower (Unit);
   begin
      return Lower_Unit = "ada"
        or else Lower_Unit = "gnat"
        or else Lower_Unit = "interfaces"
        or else Lower_Unit = "system"
        or else Lower_Unit = "calendar"
        or else Lower_Unit = "machine_code"
        or else Lower_Unit = "unchecked_conversion"
        or else Lower_Unit = "unchecked_deallocation"
        or else Lower_Unit = "direct_io"
        or else Lower_Unit = "io_exceptions"
        or else Lower_Unit = "sequential_io"
        or else Lower_Unit = "text_io"
        or else Starts_With (Lower_Unit, "ada.")
        or else Starts_With (Lower_Unit, "gnat.")
        or else Starts_With (Lower_Unit, "system.")
        or else Starts_With (Lower_Unit, "interfaces.");
   end Is_Ada_Predefined_Unit;

   -------------------------
   -- Locate_Exec_On_Path --
   -------------------------

   function Locate_Exec_On_Path (Exec_Name : String) return String is
      use GNAT;
      use type GNAT.OS_Lib.String_Access;

      Path     : OS_Lib.String_Access :=
                   OS_Lib.Locate_Exec_On_Path (Exec_Name);
      Path_Str : constant String := (if Path = null then "" else Path.all);
   begin
      OS_Lib.Free (Path);
      return Path_Str;
   end Locate_Exec_On_Path;

   ---------------------
   -- Output_Messages --
   ---------------------

   procedure Output_Messages
     (Options : GPRtools.Options.Base_Options'Class) is
   begin
      Options.Config_Project_Log.Output_Messages
        (Information    => Options.Verbosity = Very_Verbose,
         Warning        => Options.Warnings,
         Lint           => Options.Verbosity = Very_Verbose,
         Full_Path_Name => Options.Full_Path_Name_For_Brief);
      if Options.Tree /= null and then Options.Tree.Has_Messages then
         Options.Tree.all.Log_Messages.Output_Messages
           (Information    => Options.Verbosity = Very_Verbose,
            Warning        => Options.Warnings,
            Lint           => Options.Verbosity = Very_Verbose,
            Full_Path_Name => Options.Full_Path_Name_For_Brief);
      end if;
   end Output_Messages;

   ------------------
   -- Partial_Name --
   ------------------

   function Partial_Name
     (Lib_Name      : Simple_Name;
      Number        : Natural;
      Object_Suffix : Simple_Name) return Simple_Name is
   begin
      return Partial_Prefix & Lib_Name
             & Simple_Name ('_' & GNATCOLL.Utils.Image (Number, 1))
             & Object_Suffix;
   end Partial_Name;

   ----------------------
   -- Set_Program_Name --
   ----------------------

   procedure Set_Program_Name (Name : String) is
      use Ada.Environment_Variables;
      GPR_Tool : constant String := "GPR_TOOL";
   begin
      Keep_Program_Name := To_Unbounded_String (Name);

      if Value (GPR_Tool, Default => "") = "" then
         Set
           (GPR_Tool,
            (if Name in "gprclean" | "gprbuild" | "gprls"
                 | "gprinstall" | "gprdump" | "gprdoc"
             then "gprbuild"
             else Name));
      end if;
   end Set_Program_Name;

end GPRtools.Util;
