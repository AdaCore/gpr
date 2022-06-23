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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;

with GNATCOLL.Traces;
with GNATCOLL.Tribooleans;

with GPRtools.Command_Line;
with GPRtools.Util;
with GPRtools.Options;

with GPR2.Path_Name;
with GPR2.Project.Source.Artifact;
pragma Warnings (Off, "*is not referenced");
with GPR2.Project.Source.Part_Set;
pragma Warnings (On, "*is not referenced");
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.Unit_Info;
with GPR2.Unit;

procedure GPRdump is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use GNATCOLL.Tribooleans;

   use GPR2;

   type GPRdump_Options is new GPRtools.Options.Base_Options with record
      Display_Sources     : Boolean := False;
      Display_All_Sources : Boolean := False;
      Display_Artifacts   : Boolean := False;
      Display_Units       : Boolean := False;
      All_Projects        : Boolean := False;
      Source              : Unbounded_String;
   end record;

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String);

   procedure Sources (View : Project.View.Object);
   --  Display view sources

   procedure Full_Closure (Tree : Project.Tree.Object; Filename : String);

   procedure Parse_Command_Line;
   --  Parse command line parameters

   Project_Tree : GPR2.Project.Tree.Object;
   Options      : GPRdump_Options;

   ------------------
   -- Full_Closure --
   ------------------

   procedure Full_Closure (Tree : Project.Tree.Object; Filename : String) is
      File : constant GPR2.Path_Name.Object :=
               GPR2.Path_Name.Create_File
                 (Filename_Type (Filename), GPR2.Path_Name.No_Resolution);
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
            if Source.Has_Units then
               for CU of Source.Units loop
                  for S of Source.Dependencies (Index => CU.Index) loop
                     Text_IO.Put_Line
                       (S.Source.Path_Name.Value &
                        (if S.Index in Multi_Unit_Index
                           then S.Index'Image
                           else ""));
                  end loop;
               end loop;
            end if;
         end;
      end if;
   end Full_Closure;

   ---------------
   -- On_Switch --
   ---------------

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String)
   is
      pragma Unreferenced (Parser, Index);
      use type GPRtools.Command_Line.Switch_Type;
      Result : constant access GPRdump_Options :=
                 GPRdump_Options (Res.all)'Access;
   begin
      if Arg = "-s" then
         Result.Display_Sources := True;

      elsif Arg = "-a" then
         Result.Display_All_Sources := True;

      elsif Arg = "-u" then
         Result.Display_Units := True;

      elsif Arg = "-r" then
         Result.All_Projects := True;

      elsif Arg = "--artifacts" then
         Result.Display_Artifacts := True;

      elsif Arg = "-d" then
         Result.Source := To_Unbounded_String (Param);

      end if;
   end On_Switch;
   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      use GNAT.Command_Line;
      use GPRtools.Command_Line;

      Parser     : GPRtools.Options.Command_Line_Parser :=
                     GPRtools.Options.Create
                       ("2019",
                        Allow_Autoconf    => True,
                        Allow_Distributed => False);
      Dump_Group : constant GPRtools.Command_Line.Argument_Group :=
                     Parser.Add_Argument_Group ("dump",
                                                On_Switch'Unrestricted_Access);

   begin
      GPRtools.Options.Setup (GPRtools.Ls);

      Parser.Add_Argument
        (Dump_Group,
         Create ("-s", "--sources",
                 Help => "display sources"));
      Parser.Add_Argument
        (Dump_Group,
         Create ("-a", "--all-sources",
                 Help => "display all sources"));
      Parser.Add_Argument
        (Dump_Group,
         Create ("-u", "--units",
                 Help => "display units"));
      Parser.Add_Argument
        (Dump_Group,
         Create ("-r", "--recursive",
                 Help => "all non externally built project recursively"));
      Parser.Add_Argument
        (Dump_Group,
         Create ("--artifacts",
                 Help => "display compilation artifacts"));
      Parser.Add_Argument
        (Dump_Group,
         Create
           ("-d", "--depth",
            Delimiter => Space,
            Parameter => "<source>",
            Help      => "display the full closure of 'source'"));

      Options.Tree := Project_Tree.Reference;
      Parser.Get_Opt (Options);

      --  Now read arguments

      for Arg of Options.Args loop
         if not Options.Project_File.Is_Defined then
            Options.Project_File :=
              GPR2.Path_Name.Create_File (Filename_Type (Arg));
         else
            raise Invalid_Switch;
         end if;
      end loop;

      if not GPRtools.Options.Load_Project
        (Options, Absent_Dir_Error => False)
      then
         GPRtools.Command_Line.Try_Help;
      end if;
   end Parse_Command_Line;

   -------------
   -- Sources --
   -------------

   procedure Sources (View : Project.View.Object) is
      Is_Empty : Boolean := True;
   begin
      for S of View.Sources
                 (Compilable_Only => not Options.Display_All_Sources)
      loop
         Is_Empty := False;

         if Options.Display_Sources
           or else Options.Display_All_Sources
         then
            Text_IO.Put_Line (S.Path_Name.Value);
            if Options.Display_Units and then S.Has_Units then
               for U of S.Units loop
                  Text_IO.Put_Line
                    (ASCII.HT & String (U.Name)
                     & ASCII.HT & U.Kind'Img);
               end loop;
            end if;

         end if;

         if Options.Display_Artifacts then
            for A of S.Artifacts.List loop
               Text_IO.Put_Line (A.Value);
            end loop;
         end if;
      end loop;

      if Is_Empty then
         Text_IO.Put_Line ("no sources");
      end if;

      if Options.Display_Units then
         for U of View.Units loop
            Text_IO.Put_Line
              (String (U.Name) & ' '
               & (if U.Has_Spec then U.Spec.Source.Value else "-")
               & ' '
               & (if U.Has_Body then U.Main_Body.Source.Value
                 else "-")
              );
         end loop;
      end if;
   end Sources;

begin
   GNATCOLL.Traces.Parse_Config_File;
   GPRtools.Util.Set_Program_Name ("gprdump");
   Parse_Command_Line;

   if Options.Display_Sources
     or else Options.Display_All_Sources
     or else Options.Display_Artifacts
     or else Options.Display_Units
   then
      for V in Project_Tree.Iterate
        (Kind   => (Project.I_Recursive  => Options.All_Projects,
                    Project.I_Imported   => Options.All_Projects,
                    Project.I_Aggregated => Options.All_Projects,
                    others               => True),
         Status => (Project.S_Externally_Built => False),
         Filter => (Project.F_Abstract | Project.F_Aggregate => False,
                    others                                   => True))
      loop
         Sources (Project.Tree.Element (V));
      end loop;
   end if;

   if Options.Source /= Null_Unbounded_String then
      Full_Closure (Project_Tree, To_String (Options.Source));
   end if;

   for M of Project_Tree.Log_Messages.all loop
      M.Output;
   end loop;

exception
   when E : others =>
      Text_IO.Put_Line ("cannot parse project: " & Exception_Information (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);
end GPRdump;
