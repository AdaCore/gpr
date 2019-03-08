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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.OS_Lib;

with GNATCOLL.Traces;
with GNATCOLL.Tribooleans;
with GNATCOLL.Utils;

with GPR2.Containers;
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
with GPR2.Source_Reference;
with GPRtools.Options;
with GPRtools.Util;
with GPR2.Version;

procedure GPRclean is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use GNATCOLL.Tribooleans;

   use GPR2;
   use GPRtools;
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

   Dry_Run                     : aliased Boolean := False;
   All_Projects                : aliased Boolean := False;
   Remain_Useful               : aliased Boolean := False;
   No_Project                  : aliased Boolean := False;
   Unchecked_Shared_Lib_Import : aliased Boolean := False;
   Dummy                       : aliased Boolean := False;
   Debug_Mode                  : aliased Boolean := False;
   Full_Path_Name_For_Brief    : aliased Boolean := False;
   --  For not working backward compartible switches

   Mains         : GPR2.Containers.Value_Set;
   Arg_Mains     : Boolean;
   Project_Path  : Path_Name.Object;
   Project_Tree  : Project.Tree.Object;
   Context       : GPR2.Context.Object;
   Config_File   : Path_Name.Object;
   Config        : Project.Configuration.Object;
   Remove_Config : Boolean := False;
   Target        : Unbounded_String := To_Unbounded_String ("all");
   Options       : GPRtools.Options.Object; -- Common options for all tools
   Subdirs       : Unbounded_String;

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

         elsif Options.Verbose then
            Text_IO.Put_Line ("absent: " & Name);
         end if;

      else
         Delete_File (Name, Success);

         if not Options.Quiet and then Success then
            Text_IO.Put_Line ('"' & Name & """ has been deleted");

         elsif Options.Verbose and then not Success then
            Text_IO.Put_Line ('"' & Name & """ absent");
         end if;
      end if;
   end Exclude_File;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      use GNAT.Command_Line;

      Config : Command_Line_Configuration renames Options.Config;

   begin
      GPRtools.Options.Setup (Options);

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access, "-P:",
         Help => "Project file");

      Define_Switch
        (Config, No_Project'Access,
         Long_Switch => "--no-project",
         Help        => "Do not use project file");

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--target:",
         Help => "Specify a target for cross platforms");

      Define_Switch
        (Config, All_Projects'Access, "-r",
         Help => "Clean all projects recursively");

      Define_Switch
        (Options.Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--subdirs:",
         Help        => "Real obj/lib/exec dirs are subdirs",
         Argument    => "<dir>");

      Define_Switch
        (Config, Dry_Run'Access, "-n",
         Help => "Nothing to do: only list files to delete");

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access, "-X:",
         Help => "Specify an external reference for Project Files");

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--config:",
         Help => "Specify the configuration project file name");

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--autoconf:",
         Help => "Specify generated config project file name");

      Define_Switch
        (Config, Remain_Useful'Access, "-c",
         Help => "Only delete compiler generated files");

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access,
         "-aP:",
         Help => "Add directory ARG to project search path");

      Define_Switch
        (Config, Dummy'Access, "-eL",
         Help => "For backwards compatibility, has no effect");

      Define_Switch
        (Config, Unchecked_Shared_Lib_Import'Access,
         Long_Switch => "--unchecked-shared-lib-imports",
         Help => "Shared lib projects may import any project");

      Define_Switch
        (Config, Debug_Mode'Access,
         Switch => "-d",
         Help   => "Debug mode");

      Define_Switch
        (Config, Full_Path_Name_For_Brief'Access,
         Switch => "-F",
         Help   => "Full project path name in brief log messages");

      Getopt (Config);

      GPR2.Set_Debug (Debug_Mode);

      if Options.Version then
         GPR2.Version.Display
           ("GPRCLEAN", "2018", Version_String => GPR2.Version.Long_Value);
         return;
      end if;

      --  Now read arguments

      GPRtools.Options.Read_Remaining_Arguments (Project_Path, Mains);

      Arg_Mains := not Mains.Is_Empty;

      if not Project_Path.Is_Defined then
         Project_Path := Project.Look_For_Default_Project;
      elsif No_Project then
         raise Usage_Error with
           "cannot specify --no-project with a project file";
      end if;

      if not Project_Path.Is_Defined then
         Display_Help (Config);
         raise Usage_Error with "Can't determine project file to work with";
      end if;

      Options.Clean_Build_Path (Project_Path);
   end Parse_Command_Line;

   -----------------
   -- Set_Project --
   -----------------

   procedure Set_Project (Path : String) is
   begin
      if not Project_Path.Is_Defined then
         Project_Path := Project.Create (Optional_Name_Type (Path));

      else
         raise Usage_Error with
           '"' & Path & """, project already """ & Project_Path.Value & '"';
      end if;
   end Set_Project;

   -------------
   -- Sources --
   -------------

   procedure Sources (View : Project.View.Object) is
      Obj_Dir : constant Path_Name.Object := View.Object_Directory;
      Tree    : constant access Project.Tree.Object := View.Tree;

      pragma Warnings (Off);

      function "&" (Left, Right : Name_Type) return Name_Type renames GPR2."&";
      --  ??? work around a strange visibility issue

      procedure Binder_Artifacts
        (Name     : Name_Type;
         Language : Optional_Name_Type := No_Name);
      --  Add binder artefacts for the name

      ----------------------
      -- Binder_Artifacts --
      ----------------------

      procedure Binder_Artifacts
        (Name     : Name_Type;
         Language : Optional_Name_Type := No_Name)
      is
         use Ada.Text_IO;

         BF        : constant Path_Name.Full_Name :=
                       Obj_Dir.Compose
                         ((if Language = No_Name
                           then No_Name
                           else View.Binder_Prefix (Language)) & Name).Value;
         File      : File_Type;
         Generated : Boolean := False;

      begin
         if GNAT.OS_Lib.Is_Regular_File (BF) then
            Open (File, Mode => In_File, Name => BF);

            while not End_Of_File (File) loop
               declare
                  use GNATCOLL.Utils;
                  Line : constant String := Get_Line (File);
               begin
                  if Line (Line'First) = '[' then
                     Generated := Starts_With (Line, "[GENERATED ");
                  elsif Generated then
                     Exclude_File (Obj_Dir.Compose (Name_Type (Line)).Value);
                  end if;
               end;
            end loop;

            Exclude_File (BF);
            Close (File);
         end if;
      end Binder_Artifacts;

      Has_Main : constant Boolean := View.Has_Mains;
      Need_Main_Archive : Boolean := False;

   begin
      if Options.Verbose then
         Text_IO.Put_Line ("Cleaning project: """ & String (View.Name) & '"');
      end if;

      for C in View.Sources.Iterate (Project.Source.Set.S_Compilable) loop
         declare
            S : constant Project.Source.Object :=
                  Project.Source.Set.Element (C);
            Cleanup : Boolean := True;
            --  To disable cleanup if main files list exists and the main file
            --  is not from list.
            In_Mains : Boolean := False;
         begin
            if Options.Verbose then
               Text_IO.Put_Line ("source: " & S.Source.Path_Name.Value);
            end if;

            if Mains.Contains (String (S.Source.Path_Name.Simple_Name)) then
               In_Mains := True;
               Mains.Delete (String (S.Source.Path_Name.Simple_Name));
            end if;

            if Has_Main then
               if not Need_Main_Archive
                 and then S.Source.Language /= "Ada"
               then
                  Need_Main_Archive := True;
               end if;

               if S.Is_Main then
                  if Arg_Mains and then not In_Mains then
                     Cleanup := False;
                  else
                     Binder_Artifacts
                       (S.Source.Path_Name.Base_Name & ".bexch",
                        Language => S.Source.Language);
                  end if;
               end if;
            end if;

            if Cleanup then
               for F of S.Artifacts.List loop
                  Exclude_File (F.Value);
               end loop;
            end if;
         end;
      end loop;

      if Arg_Mains and then not Mains.Is_Empty then
         GPRtools.Util.Fail_Program
           ('"' & Mains.First_Element
            & """ was not found in the sources of any project");
      end if;

      if not Remain_Useful and then View.Has_Mains then
         for M of View.Mains loop
            Exclude_File (M.Value);
         end loop;
      end if;

      declare
         Lexch : constant Name_Type := ".lexch";
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

         if View.Is_Library then
            if View.Is_Aggregated_In_Library then
               Binder_Artifacts (View.Aggregate.Library_Name & Lexch);
            else
               if not Remain_Useful then
                  Exclude_File (View.Library_Filename.Value);
               end if;

               Binder_Artifacts (View.Library_Name & Lexch);
            end if;
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
            raise Usage_Error with
              "Can't split '" & Value & "' to name and value";
         end if;

         Context.Insert
           (Name_Type (Value (Value'First .. Idx - 1)),
            Value (Idx + 1 .. Value'Last));

      elsif Switch = "--config" then
         Config_File := Path_Name.Create_File (Name_Type (Normalize_Value));

      elsif Switch = "--autoconf" then
         --  --autoconf option for gprbuild mean that the file have to be
         --  generated if absent. The gprclean have to remove all gprbuild
         --  generated files.

         Remove_Config := True;

         Config_File := Path_Name.Create_File (Name_Type (Normalize_Value));

      elsif Switch = "--target" then
         Target := To_Unbounded_String (Normalize_Value);

      elsif Switch = "-aP" then
         Project_Tree.Register_Project_Search_Path
           (Path_Name.Create_Directory (Name_Type (Value)));

      elsif Switch = "--subdirs" then
         Subdirs := To_Unbounded_String (Normalize_Value);
      end if;
   end Value_Callback;

begin
   GNATCOLL.Traces.Parse_Config_File;
   GPRtools.Util.Set_Program_Name ("gprclean");
   Parse_Command_Line;

   if Options.Version then
      return;
   end if;

   if Config_File.Is_Defined then
      Config := Project.Configuration.Load
        (Config_File, Name_Type (To_String (Target)));

      if Config.Log_Messages.Has_Error then
         Util.Output_Messages
           (Config.Log_Messages, Options.Verbosity, Full_Path_Name_For_Brief);
         Util.Fail_Program
           ("Config file """ & String (Config_File.Simple_Name)
            & """ parse error");
      end if;

      Project_Tree.Load
        (Project_Path, Context, Config, Options.Build_Path,
         Optional_Name_Type (To_String (Subdirs)),
         Check_Shared_Lib => not Unchecked_Shared_Lib_Import);

   else
      Project_Tree.Load_Autoconf
        (Project_Path, Context, Options.Build_Path,
         Optional_Name_Type (To_String (Subdirs)),
         Check_Shared_Lib => not Unchecked_Shared_Lib_Import);
   end if;

   if Project_Tree.Root_Project.Is_Library and then Arg_Mains then
      Project_Tree.Log_Messages.Append
        (GPR2.Message.Create
           (GPR2.Message.Error,
            "main cannot be a source of a library project: """
            & Mains.First_Element & '"',
            GPR2.Source_Reference.Create (Project_Path.Value, 0, 0)));

      Util.Output_Messages
        (Project_Tree.Log_Messages.all, Options.Verbosity,
         Full_Path_Name_For_Brief);

      GPRtools.Util.Fail_Program ("problems with main sources");
   end if;

   for V in Project_Tree.Iterate
     (Kind   => (Project.I_Recursive  => All_Projects,
                 Project.I_Imported   => All_Projects,
                 Project.I_Aggregated => All_Projects, others => True),
      Status => (Project.S_Externally_Built => False),
      Filter => (Project.F_Abstract | Project.F_Aggregate => False,
                 others => True))
   loop
      Sources (Project.Tree.Element (V));
   end loop;

   if Remove_Config then
      Exclude_File (Config_File.Value);
   end if;

   Util.Output_Messages
     (Project_Tree.Log_Messages.all, Options.Verbosity,
      Full_Path_Name_For_Brief);

exception
   when E : GNAT.Command_Line.Exit_From_Command_Line
      | GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Invalid_Parameter
      | GPRtools.Usage_Error =>
      GPRtools.Util.Fail_Program (Exception_Message (E));

   when Project_Error =>
      GPRtools.Util.Project_Processing_Failed
        (Project_Tree, Options.Verbosity, Full_Path_Name_For_Brief);

   when E : others =>
      GPRtools.Util.Fail_Program
        ("cannot parse project: " & Exception_Information (E));
end GPRclean;
