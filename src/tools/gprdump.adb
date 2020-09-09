------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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
with GNAT.Strings;

with GNATCOLL.Traces;
with GNATCOLL.Tribooleans;

with GPRtools.Util;
with GPRtools.Options;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Source;
with GPR2.Project.Unit_Info;
with GPR2.Version;

procedure GPRdump is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use GNATCOLL.Tribooleans;

   use GPR2;

   procedure Sources (View : Project.View.Object);
   --  Display view sources

   procedure Full_Closure (Tree : Project.Tree.Object; Filename : String);

   procedure Parse_Command_Line;
   --  Parse command line parameters

   Help                : aliased Boolean := False;
   Display_Sources     : aliased Boolean := False;
   Display_All_Sources : aliased Boolean := False;
   Display_Artifacts   : aliased Boolean := False;
   Display_Units       : aliased Boolean := False;
   All_Projects        : aliased Boolean := False;
   Source              : aliased GNAT.Strings.String_Access;
   Project_Path        : Unbounded_String;
   Project_Tree        : GPR2.Project.Tree.Object;

   ------------------
   -- Full_Closure --
   ------------------

   procedure Full_Closure (Tree : Project.Tree.Object; Filename : String) is
      File : constant GPR2.Path_Name.Object :=
               GPR2.Path_Name.Create_File
                 (Name_Type (Filename), GPR2.Path_Name.No_Resolution);
      View : constant GPR2.Project.View.Object :=
               Tree.Get_View (File);
   begin
      if not View.Is_Defined then
         Text_IO.Put_Line ("view for " & Filename & " not found.");

      else
         declare
            Source : constant GPR2.Project.Source.Object :=
                       View.Source (File);
         begin
            for S of Source.Dependencies (Closure => True) loop
               Text_IO.Put_Line (S.Source.Path_Name.Value);
            end loop;
         end;
      end if;
   end Full_Closure;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      use GNAT.Command_Line;

      Options : GPRtools.Options.Object;

   begin
      Define_Switch
        (Options.Config, Help'Access,
         "-h", Long_Switch => "--help",
         Help => "display this help message and exit");

      Define_Switch
        (Options.Config, Display_Sources'Access,
         "-s", Long_Switch => "--sources",
         Help => "display sources");

      Define_Switch
        (Options.Config, Display_All_Sources'Access,
         "-a", Long_Switch => "--all-sources",
         Help => "display sources");

      Define_Switch
        (Options.Config, Display_Units'Access,
         "-u", Long_Switch => "--units",
         Help => "display units");

      Define_Switch
        (Options.Config, All_Projects'Access,
         "-r", Long_Switch => "--recoursive",
         Help => "All none external projects recoursively");

      Define_Switch
        (Options.Config, Display_Artifacts'Access,
         Long_Switch => "--artifacts",
         Help => "display artifacts");

      Define_Switch
        (Options.Config, Source'Access,
         "-d:", Long_Switch => "--deps:",
         Help => "display full closure");

      GPRtools.Util.Set_Program_Name ("gprdump");

      Getopt (Options.Config);

      if Options.Version or else Options.Verbose then
         Version.Display
           ("GPRDUMP", "2019", Version_String => Version.Long_Value);

         if Options.Version then
            Version.Display_Free_Software;
            return;
         end if;
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
         raise Invalid_Switch;
      end if;
   end Parse_Command_Line;

   -------------
   -- Sources --
   -------------

   procedure Sources (View : Project.View.Object) is
      use type GPR2.Containers.Count_Type;
      Sources_Set : constant Project.Source.Set.Object := View.Sources;
   begin
      if Sources_Set.Length = 0 then
         Text_IO.Put_Line ("no sources");

      else
         for C in Sources_Set.Iterate
           (Filter => (if Display_All_Sources
                       then GPR2.Project.Source.Set.S_All
                       else GPR2.Project.Source.Set.S_Compilable))
         loop
            declare
               S : constant Project.Source.Object := Sources_Set (C);
            begin
               if Display_Sources or Display_All_Sources then
                  Text_IO.Put_Line (S.Source.Path_Name.Value);
                  if Display_Units and then S.Source.Has_Units then
                     for U of S.Source.Units loop
                        Text_IO.Put_Line
                          (ASCII.HT & String (U.Name)
                           & ASCII.HT & U.Kind'Img);
                     end loop;
                  end if;

               end if;

               if Display_Artifacts then
                  for A of S.Artifacts.List loop
                     Text_IO.Put_Line (A.Value);
                  end loop;
               end if;
            end;
         end loop;

         if Display_Units then
            for U of View.Units loop
               Text_IO.Put_Line
                 (String (U.Name) & ' '
                  & (if U.Has_Spec then U.Spec.Value else "-")
                  & ' '
                  & (if U.Has_Body then U.Main_Body.Value
                     else "-")
                 );
            end loop;
         end if;
      end if;
   end Sources;

begin
   GNATCOLL.Traces.Parse_Config_File;
   GPRtools.Util.Set_Program_Name ("gprdump");
   Parse_Command_Line;

   declare
      use type GNAT.Strings.String_Access;

      Pathname : constant GPR2.Path_Name.Object :=
                   GPR2.Project.Create
                     (GPR2.Optional_Name_Type (To_String (Project_Path)));
      Context  : GPR2.Context.Object;
   begin
      Project_Tree.Load (Pathname, Context, Check_Shared_Lib => False);

      if Display_Sources or else Display_All_Sources or else Display_Artifacts
        or else Display_Units
      then
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
      end if;

      if Source /= null and then Source.all /= "" then
         Full_Closure (Project_Tree, Source.all);
      end if;

   exception
      when E : others =>
         Text_IO.Put_Line
           ("error while parsing..." & Exception_Information (E));
   end;

   for M of Project_Tree.Log_Messages.all loop
      Text_IO.Put_Line (M.Format);
   end loop;

exception
   when GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Exit_From_Command_Line
      =>
      Command_Line.Set_Exit_Status (Command_Line.Failure);

   when E : others =>
      Text_IO.Put_Line ("cannot parse project: " & Exception_Information (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);
end GPRdump;
