------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2017-2018, Free Software Foundation, Inc.          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;

with Ada.Directories;
with GNAT.OS_Lib;

with GPR2.Message;
with GPR2.Parser.Project;
with GPR2.Project.Definition;

package body GPR2.Project.Configuration is

   ------------------------
   -- Corresponding_View --
   ------------------------

   function Corresponding_View (Self : Object) return Project.View.Object is
   begin
      return Self.Conf;
   end Corresponding_View;

   ------------
   -- Create --
   ------------

   function Create
     (Language : Name_Type;
      Version  : Optional_Name_Type := No_Name;
      Runtime  : Optional_Name_Type := No_Name;
      Path     : Optional_Name_Type := No_Name;
      Name     : Optional_Name_Type := No_Name) return Description
   is
      function "+" (Str : Optional_Name_Type) return Unbounded_String
        is (To_Unbounded_String (String (Str)));
   begin
      return Description'
        (Language => +Language,
         Version  => +Version,
         Runtime  => +Runtime,
         Path     => +Path,
         Name     => +Name);
   end Create;

   function Create
     (Filename : Path_Name.Object;
      Target   : Name_Type := "all") return Object
   is
      Result  : Object;
      Project : constant Parser.Project.Object :=
                  Parser.Project.Load (Filename, Result.Messages);
      Data    : GPR2.Project.Definition.Data (Has_Context => False);
   begin
      --  Continue only if there is no parsing error on the configuration
      --  project.

      Data.Trees.Project := Project;
      Data.Context_View  := View.Undefined;
      Data.Status        := Definition.Root;
      Data.Kind          := K_Configuration;

      Result.Conf         := Definition.Register (Data);
      Result.Target       :=
        (if Target = "all"
         then Null_Unbounded_String
         else To_Unbounded_String (String (Target)));

      return Result;
   end Create;

   ------------------
   -- Has_Messages --
   ------------------

   function Has_Messages (Self : Object) return Boolean is
   begin
      return not Self.Messages.Is_Empty;
   end Has_Messages;

   ----------
   -- Load --
   ----------

   function Load
     (Settings : Description_Set;
      Target   : Name_Type := "all") return Object
   is
      --  Note that this is a temporary implementation to bring a solution
      --  for the configuration support in LibGPR2. The long term and proper
      --  solution will be implemented later based on the knowledge base API
      --  which could be reviewed from scratch.

      use Ada;
      use GNAT;
      use type Path_Name.Object;

      function Process_Id return String is
        (Strings.Fixed.Trim
           (Integer'Image (OS_Lib.Pid_To_Integer (OS_Lib.Current_Process_Id)),
            Strings.Both));

      --  Note that Temporary_Directory_Path could be the empty string and in
      --  this case we just write the temporary file into the current working
      --  directory.

      Conf_Filename : constant String :=
                        (if Path_Name.Temporary_Directory = Path_Name.Undefined
                         then ""
                         else Path_Name.Compose
                           (Path_Name.Temporary_Directory,
                            Name_Type
                              (Process_Id & "-gpr2_tmp_conf.cgpr")).Value);

      GPRconfig : constant OS_Lib.String_Access :=
                    OS_Lib.Locate_Exec_On_Path ("gprconfig");
      Args      : OS_Lib.Argument_List (1 .. Settings'Length + 5);
      Success   : Boolean := False;

      Result    : Object;
   begin
      --  Build parameters

      Args (1) := GPRconfig;
      Args (2) := new String'("-o");
      Args (3) := new String'(Conf_Filename);
      Args (4) := new String'("--batch");
      Args (5) := new String'("--target=" & String (Target));

      for K in Settings'Range loop
         Args (6 + K - Settings'First) :=
           new String'("--config="
                       & To_String (Settings (K).Language)
                       & "," & To_String (Settings (K).Version)
                       & "," & To_String (Settings (K).Runtime)
                       & "," & To_String (Settings (K).Path)
                       & "," & To_String (Settings (K).Name));
      end loop;

      --  Execute external GPRconfig tool

      OS_Lib.Spawn (GPRconfig.all, Args, Success);

      --  Free arguments

      for K in Args'Range loop
         OS_Lib.Free (Args (K));
      end loop;

      --  Load the configuration object generated if execution was succeful

      Result.Conf := View.Undefined;

      if Success then
         Result := Create (Create (Name_Type (Conf_Filename)), Target);

         for S of Settings loop
            Result.Descriptions.Append (S);
         end loop;

      else
         Result.Messages.Append
           (Message.Create
              (Message.Error,
               "cannot create configuration file, fail to execute gprconfig"));
      end if;

      if Directories.Exists (Conf_Filename) then
         Directories.Delete_File (Conf_Filename);
      end if;

      return Result;
   end Load;

   ------------------
   -- Log_Messages --
   ------------------

   function Log_Messages (Self : Object) return Log.Object is
   begin
      return Self.Messages;
   end Log_Messages;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Object) is
   begin
      Self.Conf.Release;
      Self := Undefined;
   end Release;

   -------------
   -- Runtime --
   -------------

   function Runtime
     (Self : Object; Language : Name_Type) return Optional_Name_Type is
   begin
      for Description of Self.Descriptions loop
         if Optional_Name_Type (To_String (Description.Language))
           = Language
         then
            return Optional_Name_Type (To_String (Description.Runtime));
         end if;
      end loop;

      return "";
   end Runtime;

   ------------
   -- Target --
   ------------

   function Target (Self : Object) return Optional_Name_Type is
   begin
      return Optional_Name_Type (To_String (Self.Target));
   end Target;

end GPR2.Project.Configuration;
