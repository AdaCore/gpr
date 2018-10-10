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
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.OS_Lib;

with GNATCOLL.Traces;

with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
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

   use GPR2;

   procedure Sources (View : Project.View.Object);
   --  Display view sources

   procedure Parse_Command_Line;
   --  Parse command line parameters

   procedure Exclude_File (Name : String);
   --  Remove file if exists

   Version      : aliased Boolean := False;
   Verbose      : aliased Boolean := False;
   Dry_Run      : aliased Boolean := False;
   Project_Path : Unbounded_String;
   Project_Tree : Project.Tree.Object;

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

         if Verbose then
            Text_IO.Put_Line
              ((if Success then "removed" else "absent") & ": " & Name);
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
        (Config, Dry_Run'Access, "-n",
         Help => "Nothing to do: only list files to delete");

      Set_Usage (Config, Usage => "[switches] <project>");

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

            if Project_Path = Null_Unbounded_String then
               Project_Path := To_Unbounded_String (Arg);
            else
               raise Invalid_Switch;
            end if;
         end;
      end loop Read_Arguments;

      if Project_Path = Null_Unbounded_String then
         Display_Help (Config);
         raise Invalid_Switch;
      end if;
   end Parse_Command_Line;

   -------------
   -- Sources --
   -------------

   procedure Sources (View : Project.View.Object) is
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
         end;
      end loop;

      for M of View.Mains loop
         Exclude_File (M.Value);
      end loop;
   end Sources;

begin
   GNATCOLL.Traces.Parse_Config_File;
   Parse_Command_Line;

   if not Version then
      declare
         Pathname : constant Path_Name.Object :=
                      Project.Create
                        (Optional_Name_Type (To_String (Project_Path)));
         Context  : GPR2.Context.Object;
      begin
         Project_Tree.Load (Pathname, Context);
         Sources (Project_Tree.Root_Project);
      end;

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
