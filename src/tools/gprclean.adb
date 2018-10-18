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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.OS_Lib;

with GNATCOLL.Traces;
with GNATCOLL.Tribooleans;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Configuration;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Source;
with GPR2.Version;

procedure GPRclean is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use GNATCOLL.Tribooleans;

   use GPR2;
   use GPR2.Path_Name;

   procedure Sources (View : Project.View.Object);
   --  Display view sources

   procedure Parse_Command_Line;
   --  Parse command line parameters

   procedure Exclude_File (Name : String);
   --  Remove file if exists

   procedure Value_Callback (Switch, Value : String);
   --  Accept string swithces

   procedure Set_Project (Path : String);
   --  Set project pathname, raise exception if already done

   Version       : aliased Boolean := False;
   Verbose       : aliased Boolean := False;
   Dry_Run       : aliased Boolean := False;
   Quiet_Output  : aliased Boolean := False;
   All_Projects  : aliased Boolean := False;
   Remain_Useful : aliased Boolean := False;
   Project_Path  : Path_Name.Object;
   Project_Tree  : Project.Tree.Object;
   Context       : GPR2.Context.Object;
   Config_File   : Path_Name.Object;
   Config        : Project.Configuration.Object;
   Target        : Unbounded_String := To_Unbounded_String ("all");
   Config_Error  : Boolean := False;

   Binder_Prefix : constant Name_Type := "b__";

   ------------------
   -- Exclude_File --
   ------------------

   procedure Exclude_File (Name : String) is
      use GNAT.OS_Lib;
      Success : Boolean;
   begin
      if Dry_Run then
         if Is_Regular_File (Name) then
            Text_IO.Put_Line (Name);
         elsif Verbose then
            Text_IO.Put_Line ("absent: " & Name);
         end if;

      else
         Delete_File (Name, Success);

         if not Quiet_Output and then Success then
            Text_IO.Put_Line ('"' & Name & """ has beed deleted");

         elsif Verbose and then not Success then
            Text_IO.Put_Line ('"' & Name & """ absent");
         end if;
      end if;
   end Exclude_File;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      use GNAT.Command_Line;

      Config : Command_Line_Configuration;

   begin
      Define_Switch
        (Config, Version'Access, Long_Switch => "--version",
         Help => "Display version and exit");

      Define_Switch
        (Config, "-h", Long_Switch => "--help",
         Help => "Display this help message and exit");

      Define_Switch
        (Config, Verbose'Access, "-v", Long_Switch => "--verbose",
         Help => "Verbose mode");

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access, "-P:",
         Help => "Project file");

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--target:",
         Help => "Specify a target for cross platforms");

      Define_Switch
        (Config, All_Projects'Access, "-r",
         Help => "Clean all projects recursively");

      Define_Switch
        (Config, Dry_Run'Access, "-n",
         Help => "Nothing to do: only list files to delete");

      Define_Switch
        (Config, Quiet_Output'Access, "-q",
         Help => "Be quiet/terse");

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access, "-X:",
         Help => "Specify an external reference for Project Files");

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--config:",
         Help => "Specify the configuration project file name");

      Define_Switch
        (Config, Remain_Useful'Access, "-c",
         Help => "Only delete compiler-generated files");

      Getopt (Config);

      if Version then
         GPR2.Version.Display
           ("GPRCLEAN", "2018", Version_String => GPR2.Version.Long_Value);
         return;
      end if;

      --  Now read arguments

      Read_Arguments : loop
         declare
            Arg : constant String := Get_Argument;
         begin
            exit Read_Arguments when Arg = "";

            Set_Project (Arg);
         end;
      end loop Read_Arguments;

      if Project_Path = Undefined then
         Display_Help (Config);
         raise Invalid_Switch;
      end if;
   end Parse_Command_Line;

   -----------------
   -- Set_Project --
   -----------------

   procedure Set_Project (Path : String) is
   begin
      if Project_Path = Undefined then
         Project_Path := Project.Create (Optional_Name_Type (Path));

      else
         raise GNAT.Command_Line.Invalid_Switch with
           '"' & Path & """, project already """ & Project_Path.Value & '"';
      end if;
   end Set_Project;

   -------------
   -- Sources --
   -------------

   procedure Sources (View : Project.View.Object) is
      Obj_Dir : constant Path_Name.Object := View.Object_Directory;
      Tree    : constant access Project.Tree.Object := View.Tree;

      procedure Binder_Artifacts (Base_Name : Name_Type);
      --  Add binder artefacts for the name

      procedure Add_Main_Artifacts (Src : Project.Source.Object);
      --  Add main artefacts if source is main

      ------------------------
      -- Add_Main_Artifacts --
      ------------------------

      procedure Add_Main_Artifacts (Src : Project.Source.Object) is
         Base : constant Name_Type := Src.Source.Path_Name.Base_Name;
      begin
         Exclude_File (Obj_Dir.Compose (Base).Value & ".bexch");
         Binder_Artifacts (Base);
      end Add_Main_Artifacts;

      ----------------------
      -- Binder_Artifacts --
      ----------------------

      procedure Binder_Artifacts (Base_Name : Name_Type) is
         PB : constant Path_Name.Full_Name :=
                Obj_Dir.Compose (Binder_Prefix & Base_Name).Value;
      begin
         Exclude_File (PB & ".ads");
         Exclude_File (PB & ".adb");
         Exclude_File (PB & Value_Type (Tree.Object_Suffix));
         Exclude_File (PB & Value_Type (Tree.Dependency_Suffix));
      end Binder_Artifacts;

      Has_Main : constant Boolean := View.Has_Mains;
      Need_Main_Archive : Boolean := False;

   begin
      for C in View.Sources.Iterate (Project.Source.Set.S_Compilable) loop
         declare
            S : constant Project.Source.Object :=
                  Project.Source.Set.Element (C);
         begin
            if Verbose then
               Text_IO.Put_Line ("source: " & S.Source.Path_Name.Value);
            end if;

            for F of S.Artifacts.List loop
               Exclude_File (F.Value);
            end loop;

            if Has_Main then
               if not Need_Main_Archive
                 and then S.Source.Language /= "Ada"
               then
                  Need_Main_Archive := True;
               end if;

               if S.Is_Main then
                  Add_Main_Artifacts (S);
               end if;
            end if;
         end;
      end loop;

      if not Remain_Useful and then View.Has_Mains then
         for M of View.Mains loop
            Exclude_File (M.Value);
         end loop;
      end if;

      declare
         GI_DB : constant Name_Type := "gnatinspect.db";
      begin
         Exclude_File (Obj_Dir.Compose (GI_DB).Value);
         Exclude_File (Obj_Dir.Compose (GI_DB & "-shm").Value);
         Exclude_File (Obj_Dir.Compose (GI_DB & "-wal").Value);

         if Need_Main_Archive then
            declare
               Main_Lib : constant Value_Type :=
                            Obj_Dir.Compose
                              ("lib" & View.Path_Name.Base_Name).Value;
            begin
               Exclude_File (Main_Lib & String (Tree.Archive_Suffix));
               Exclude_File (Main_Lib & ".deps");
            end;
         end if;

         if View.Kind in K_Library | K_Aggregate_Library then
            declare
               Lib_Name : constant Name_Type := View.Library_Name;
            begin
               if not Remain_Useful then
                  Exclude_File
                    (View.Library_Directory.Compose ("lib" & Lib_Name).Value
                     & Value_Type (Project_Tree.Archive_Suffix));
               end if;
               Exclude_File (Obj_Dir.Compose (Lib_Name).Value & ".lexch");

               if View.Is_Library_Standalone then
                  Binder_Artifacts (Lib_Name);
               end if;
            end;
         end if;
      end;
   end Sources;

   --------------------
   -- Value_Callback --
   --------------------

   procedure Value_Callback (Switch, Value : String) is
      Idx : Natural;

      function Normalize_Value return String is
        (if Value /= "" and then Value (Value'First) = '='
         then Value (Value'First + 1 .. Value'Last) else Value);
      --  Remove leading '=' symbol from value for options like
      --  --config=file.cgrp

   begin
      if Switch = "-P" then
         Set_Project (Value);

      elsif Switch = "-X" then
         Idx := Ada.Strings.Fixed.Index (Value, "=");

         if Idx = 0 then
            raise GNAT.Command_Line.Invalid_Switch with
              "Can't split '" & Value & "' to name and value";
         end if;

         Context.Insert
           (Name_Type (Value (Value'First .. Idx - 1)),
            Value (Idx + 1 .. Value'Last));

      elsif Switch = "--config" then
         Config_File := Path_Name.Create_File (Name_Type (Normalize_Value));

      elsif Switch = "--target" then
         Target := To_Unbounded_String (Normalize_Value);
      end if;
   end Value_Callback;

begin
   GNATCOLL.Traces.Parse_Config_File;
   Parse_Command_Line;

   if not Version then
      if Config_File /= Undefined then
         Config := Project.Configuration.Load
           (Config_File, Name_Type (To_String (Target)));

         if Config.Has_Messages then
            for M of Config.Log_Messages loop
               case M.Level is
                  when Message.Information =>
                     if Verbose then
                        Text_IO.Put_Line (M.Format);
                     end if;
                  when Message.Warning =>
                     if not Quiet_Output then
                        Text_IO.Put_Line (M.Format);
                     end if;
                  when Message.Error =>
                     Text_IO.Put_Line (M.Format);
                     Config_Error := True;
               end case;
            end loop;
         end if;
      end if;

      if Config_Error then
         return;
      end if;

      Project_Tree.Load (Project_Path, Context, Config);

      for V in Project_Tree.Iterate
        (Kind   => (Project.I_Recursive => All_Projects,
                    Project.I_Imported  => All_Projects, others => True),
         Status => (Project.S_Externally_Built => False))
      loop
         Sources (Project.Tree.Element (V));
      end loop;

      if Verbose then
         for M of Project_Tree.Log_Messages.all loop
            Text_IO.Put_Line (M.Format);
         end loop;
      end if;
   end if;

exception
   when GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Exit_From_Command_Line
      =>
      Command_Line.Set_Exit_Status (Command_Line.Failure);

   when Project_Error =>
      if Verbose then
         --  Display all messagges
         for M of Project_Tree.Log_Messages.all loop
            Text_IO.Put_Line (M.Format);
         end loop;

      else
         --  Display only errors
         for C in Project_Tree.Log_Messages.Iterate
           (False, False, True, True, True)
         loop
            Text_IO.Put_Line (Log.Element (C).Format);
         end loop;
      end if;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : others =>
      Text_IO.Put_Line ("cannot parse project: " & Exception_Information (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);
end GPRclean;
