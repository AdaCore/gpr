------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2022-2024, AdaCore                     --
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
with Ada.Containers;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib;

with GNATCOLL.Traces;

with GPR2.Build.Actions_Population;
with GPR2.Build.Compilation_Unit;
with GPR2.Build.External_Options;
with GPR2.Build.Process_Manager.JSON;
with GPR2.Build.Source;
with GPR2.Interrupt_Handler;
with GPR2.Message;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Source_Reference;

with GPRtools.Options;
with GPRtools.Program_Termination;
with GPRtools.Sigint;
with GPRtools.Util;

with GPRbuild.Options;

-------------------
-- GPRbuild.Main --
-------------------

function GPRbuild.Main return Ada.Command_Line.Exit_Status is

   use Ada;
   use type Ada.Containers.Count_Type;
   use Ada.Exceptions;

   use GPR2;
   use GPRtools;
   use GPRtools.Program_Termination;

   package PRP renames GPR2.Project.Registry.Pack;
   package PRA renames GPR2.Project.Registry.Attribute;

   function Has_Absolute_Artifacts_Dir
     (View : GPR2.Project.View.Object) return Boolean;

   function Ensure_Directories
     (Tree : GPR2.Project.Tree.Object) return Boolean;
   --  Ensures all obj/lib/exec directories are in place. If such a directory
   --  is missing then:
   --  * if Opt.Create_Missing_Dirs is set, it will try to create them, and
   --    report an error and return False if such creation was not successful.
   --  * if the project is simple enough (only relative path, no
   --    non-externally-built dependency, and simulation is not set, then it
   --    is created
   --  * if simulation is set, then it is not created, but the funciton will
   --    return False upon non-simple project requiring creating of directories

   function Execute
     (PM : in out GPR2.Build.Process_Manager.Object'Class)
      return Command_Line.Exit_Status;

   Opt  : Options.Object;
   Tree : Project.Tree.Object;


   ------------------------
   -- Ensure_Directories --
   ------------------------

   function Ensure_Directories
     (Tree : GPR2.Project.Tree.Object) return Boolean
   is
      procedure Ensure (Path : GPR2.Path_Name.Object);
      --  Make sure Path exists and is a directory.

      function Mkdir_Recursive (Path : GPR2.Path_Name.Object) return Boolean;
      --  Creates Path recursively

      All_Ok : Boolean := True;
      Force  : Boolean := False;

      ------------
      -- Ensure --
      ------------

      procedure Ensure (Path : GPR2.Path_Name.Object)
      is
         function Path_Img return String;

         --------------
         -- Path_Img --
         --------------

         function Path_Img return String is
            Relative : constant Filename_Type :=
                         Path.Relative_Path (Tree.Root_Project.Dir_Name);
            Absolute : constant Filename_Type := Path.Value;
         begin
            if Relative'Length < Absolute'Length then
               return String (Relative);
            else
               return String (Absolute);
            end if;
         end Path_Img;

      begin
         if not Path.Exists then
            if Opt.Create_Missing_Dirs or else Force then
               if Mkdir_Recursive (Path) then
                  Tree.Reporter.Report ('"' & Path_Img & """ created");
               else
                  All_Ok := False;
               end if;
            else
               Handle_Program_Termination
                 (Force_Exit => False,
                  Message    => '"' & Path_Img & """ does not exist");
               All_Ok := False;
            end if;
         end if;
      end Ensure;

      ---------------------
      -- Mkdir_Recursive --
      ---------------------

      function Mkdir_Recursive (Path : GPR2.Path_Name.Object) return Boolean is
         Parent : constant GPR2.Path_Name.Object :=
                    Path.Containing_Directory;
      begin
         if not Parent.Exists
           and then not Mkdir_Recursive (Parent)
         then
            return False;
         end if;

         Ada.Directories.Create_Directory (Path.String_Value);

         return True;

      exception
         when Ada.IO_Exceptions.Use_Error =>
            Tree.Reporter.Report
              ("error: could not create directory """ &
                 Path.String_Value & '"',
               To_Stderr => True,
               Level     => GPR2.Message.Important);
            return False;
      end Mkdir_Recursive;

      use type GPR2.Path_Name.Object;

   begin
      --  gprbuild creates obj/lib/exec dirs without requiring -p in case of
      --  "simple" project tree: no aggregate root project, root project
      --  with no dependencies (or externally built ones), simple relative
      --  directories

      Force := False;

      if Tree.Root_Project.Kind /= K_Aggregate
        and then not Has_Absolute_Artifacts_Dir (Tree.Root_Project)
      then
         Force := True;

         for V of Tree.Root_Project.Closure loop
            if not V.Is_Externally_Built
              and then V.Kind /= K_Abstract
            then
               Force := False;
               exit;
            end if;
         end loop;
      end if;

      for V of Tree.Ordered_Views loop
         if not V.Is_Externally_Built then
            if V.Kind in GPR2.With_Object_Dir_Kind then
               Ensure (V.Object_Directory);

               if V.Kind = K_Standard
                 and then V.Is_Namespace_Root
                 and then V.Executable_Directory /= V.Object_Directory
               then
                  Ensure (V.Executable_Directory);
               end if;
            end if;

            if V.Is_Library then
               Ensure (V.Library_Directory);
               Ensure (V.Library_Ali_Directory);
            end if;
         end if;
      end loop;

      return All_Ok;
   end Ensure_Directories;

   -------------
   -- Execute --
   -------------

   function Execute
     (PM : in out GPR2.Build.Process_Manager.Object'Class)
      return Command_Line.Exit_Status is
   begin
      if not Tree.Artifacts_Database.Execute
        (PM, Opt.PM_Options)
      then
         return To_Exit_Status (E_Errors);
      end if;

      return To_Exit_Status (E_Success);
   end Execute;

   --------------------------------
   -- Has_Absolute_Artifacts_Dir --
   --------------------------------

   function Has_Absolute_Artifacts_Dir
     (View : GPR2.Project.View.Object) return Boolean
   is
      function Check_Absolute
        (Attr : GPR2.Project.Attribute.Object) return Boolean;

      --------------------
      -- Check_Absolute --
      --------------------

      function Check_Absolute
        (Attr : GPR2.Project.Attribute.Object) return Boolean is
      begin
         if Attr.Is_Defined
           and then GNAT.OS_Lib.Is_Absolute_Path
             (Attr.Value.Text)
         then
            return True;
         else
            return False;
         end if;
      end Check_Absolute;

   begin
      if View.Is_Externally_Built then
         --  Ignore
         return False;
      end if;

      if View.Kind in GPR2.With_Object_Dir_Kind then
         if Check_Absolute (View.Attribute (PRA.Object_Dir)) then
            return True;
         end if;

         if View.Kind = K_Standard
           and then View.Is_Namespace_Root
         then
            if Check_Absolute (View.Attribute (PRA.Exec_Dir)) then
               return True;
            end if;
         end if;
      end if;

      if View.Is_Library then
         if Check_Absolute (View.Attribute (PRA.Library_Dir)) then
            return True;
         end if;

         if Check_Absolute (View.Attribute (PRA.Library_Ali_Dir)) then
            return True;
         end if;
      end if;

      return False;
   end Has_Absolute_Artifacts_Dir;

   Parser         : constant Options.GPRbuild_Parser := Options.Create;
   Sw_Attr        : GPR2.Project.Attribute.Object;
   Process_M      : GPR2.Build.Process_Manager.Object;
   Process_M_JSON : GPR2.Build.Process_Manager.JSON.Object;
   Jobs_JSON      : GPR2.Path_Name.Object;

   use GPR2.Build;

begin

   GNATCOLL.Traces.Parse_Config_File;

   --  Install the Ctrl-C handler

   GPR2.Interrupt_Handler.Install_Sigint (GPRtools.Sigint.Handler'Access);

   --  Set program name

   GPRtools.Util.Set_Program_Name ("gprbuild");

   --  Parse arguments

   Parser.Get_Opt (Opt);

   --  Load the project tree and sources

   if not GPRtools.Options.Load_Project
     (Opt,
      Absent_Dir_Error => GPR2.No_Error,
      Restricted_To_Languages => Opt.Restricted_To_Languages)
   then
      Handle_Program_Termination (Message => "");
   end if;

   Tree := Opt.Tree;

   if not Tree.Update_Sources (Option => Sources_Units_Artifacts) then
      Handle_Program_Termination
        (Force_Exit => True,
         Exit_Cause => E_Tool,
         Message    => "processing failed");
      return To_Exit_Status (E_Fatal);
   end if;

   if not Ensure_Directories (Tree) then
      Handle_Program_Termination (Force_Exit => False);

      return To_Exit_Status (E_Abort);
   end if;

   --  Check if we have a Builder'Switches attribute in the root project

   if Tree.Root_Project.Has_Package (PRP.Builder)
     and then not Tree.Root_Project.Attributes
       (PRA.Builder.Switches).Is_Empty
   then
      declare
         Has_Error : Boolean := False;
         Mains : constant Compilation_Unit.Unit_Location_Vector :=
                   (if not Opt.Build_Options.Mains.Is_Empty
                    then Actions_Population.Resolve_Mains
                      (Tree, Opt.Build_Options, Has_Error)
                    elsif Tree.Root_Project.Has_Mains
                    then Tree.Root_Project.Mains
                    else GPR2.Build.Compilation_Unit.Empty_Vector);
      begin
         if Has_Error then
            Handle_Program_Termination
              (Force_Exit => True,
               Exit_Cause => E_Tool,
               Message    => "processing failed");
            return To_Exit_Status (E_Fatal);
         end if;

         --  #1: If one main is defined, from the Main top-level attribute or
         --  from the command line, we fetch Builder'Switches(<main>).

         if Mains.Length = 1 then
            declare
               Source_Part : constant Compilation_Unit.Unit_Location :=
                               Mains.First_Element;
            begin
               Sw_Attr := Tree.Root_Project.Attribute
                 (Name   => PRA.Builder.Switches,
                  Index  => Project.Attribute_Index.Create
                    (GPR2.Value_Type (Source_Part.Source.Simple_Name),
                     Case_Sensitive => GPR2.File_Names_Case_Sensitive),
                  At_Pos => Source_Part.Index);
            end;
         end if;

         --  #2: case where all mains have the same language

         if not Sw_Attr.Is_Defined
           and then not Mains.Is_Empty
         then
            declare
               Lang : GPR2.Language_Id := No_Language;
               Src  : GPR2.Build.Source.Object;
            begin
               for Main of Mains loop
                  Src := Tree.Root_Project.Source (Main.Source.Simple_Name);

                  if Src.Is_Defined then
                     if Lang = No_Language then
                        Lang := Src.Language;

                     elsif Lang /= Src.Language then
                        --  Mains with different languages
                        Lang := No_Language;

                        exit;
                     end if;
                  else
                     Lang := No_Language;

                     exit;
                  end if;
               end loop;

               if Lang /= No_Language then
                  Sw_Attr := Tree.Root_Project.Attribute
                    (Name  => PRA.Builder.Switches,
                     Index => Project.Attribute_Index.Create (Lang));
               end if;
            end;
         end if;

         --  #3 check languages of the root project if no main is defined

         if not Sw_Attr.Is_Defined
           and then Mains.Is_Empty
         then
            declare
               Lang        : Language_Id := No_Language;
               Driver_Attr : GPR2.Project.Attribute.Object;
               New_Lang    : Language_Id;
            begin
               for Val of Tree.Root_Project.Languages loop
                  New_Lang := +Optional_Name_Type (Val.Text);

                  Driver_Attr := Tree.Root_Project.Attribute
                    (Name  => PRA.Compiler.Driver,
                     Index => Project.Attribute_Index.Create (New_Lang));

                  if Driver_Attr.Is_Defined then
                     if Lang = No_Language then
                        Lang := New_Lang;
                     elsif Lang /= New_Lang then
                        Lang := No_Language;
                        exit;
                     end if;
                  end if;
               end loop;

               if Lang /= No_Language then
                  Sw_Attr := Tree.Root_Project.Attribute
                    (Name  => PRA.Builder.Switches,
                     Index => Project.Attribute_Index.Create (Lang));
               end if;
            end;
         end if;

         --  #4 check Switches (others)

         if not Sw_Attr.Is_Defined then
            Sw_Attr := Tree.Root_Project.Attribute
              (Name  => PRA.Builder.Switches,
               Index => Project.Attribute_Index.I_Others);
         end if;

         if not Sw_Attr.Is_Defined
           and then Mains.Length > 1
         then
            Tree.Reporter.Report
              ("warning: Builder'Switches attribute is ignored as there are" &
                 " several mains");
         end if;
      end;

      --  Finally, if we found a Switches attribute, apply it

      if Sw_Attr.Is_Defined then
         Opt := Options.Object'(GPRtools.Options.Empty_Options
                                with others => <>);
         Opt.Tree := Tree;
         Parser.Get_Opt (From_Pack => PRP.Builder,
                         Values    => Sw_Attr.Values,
                         Result    => Opt);
         Parser.Get_Opt (Opt);
      end if;
   end if;

   --  Handle Builder'Global_Compilation_Switches

   if Tree.Root_Project.Has_Package (PRP.Builder)
     and then not Tree.Root_Project.Attributes
       (PRA.Builder.Global_Compilation_Switches).Is_Empty
   then
      for Attr of Tree.Root_Project.Attributes
        (PRA.Builder.Global_Compilation_Switches)
      loop
         declare
            Lang : constant GPR2.Language_Id :=
                     GPR2."+" (Name_Type (Attr.Index.Value));
         begin
            for V of Attr.Values loop
               Opt.Extra_Args.Register
                 (GPR2.Build.External_Options.Compiler,
                  Lang,
                  V.Text);
            end loop;
         end;
      end loop;
   end if;

   if Opt.Dash_A_Option then
      Opt.Console_Reporter.Report
        ("warning: switch -a is ignored and no additional source is compiled",
         To_Stderr => True,
         Level     => GPR2.Message.Important);
   end if;

   if Opt.No_Split_Units then
      declare
         use type GPR2.Project.View.Object;
         Has_Errors : Boolean := False;
      begin
         for V of Opt.Tree.Namespace_Root_Projects loop
            for U of V.Units loop
               if U.Has_Part (S_Spec)
                 and then U.Spec.View /= U.Owning_View
               then
                  Opt.Tree.Reporter.Report
                    (GPR2.Message.Create
                       (GPR2.Message.Error,
                        "the spec for unit """ & String (U.Name) &
                          """ does not belong to the view """ &
                          String (U.Owning_View.Name) &
                          """ that defines the body",
                        GPR2.Source_Reference.Create
                          (U.Spec.View.Path_Name.Value, 0, 0)));
                  Has_Errors := True;
               end if;

               for Sep of U.Separates loop
                  if Sep.View /= U.Owning_View then
                     Opt.Tree.Reporter.Report
                       (GPR2.Message.Create
                          (GPR2.Message.Error,
                           "the separate """ &
                             String (Sep.Source.Simple_Name) &
                             """ for unit """ & String (U.Name) &
                             """ does not belong to the view """ &
                             String (U.Owning_View.Name) &
                             """ that defines the body",
                           GPR2.Source_Reference.Create
                             (Sep.View.Path_Name.Value, 0, 0)));
                     Has_Errors := True;
                  end if;
               end loop;
            end loop;
         end loop;

         if Has_Errors then
            Handle_Program_Termination
              (Force_Exit => True,
               Exit_Cause => E_Tool,
               Message    => "processing failed");
            return To_Exit_Status (E_Errors);
         end if;
      end;
   end if;

   --  Set user-specified cargs/bargs/largs if any

   Opt.Tree.Artifacts_Database.Set_External_Options (Opt.Extra_Args);

   --  Now populate the Build database's actions

   if not GPR2.Build.Actions_Population.Populate_Actions
     (Tree, Opt.Build_Options)
   then
      Handle_Program_Termination
        (Force_Exit => True,
         Exit_Cause => E_Tool,
         Message    => "processing failed");
      return To_Exit_Status (E_Fatal);
   end if;

   if Opt.Json_Summary then
      Jobs_JSON := Tree.Root_Project.Dir_Name.Compose ("jobs.json");
      Process_M_JSON.Set_JSON_File (Jobs_JSON);

      return Execute (Process_M_JSON);
   else
      return Execute (Process_M);
   end if;

exception
   when E : GPR2.Options.Usage_Error =>
      Handle_Program_Termination
        (Display_Command_Line_Help => True,
         Force_Exit                => False,
         Message                   => Exception_Message (E));
      return To_Exit_Status (E_Fatal);

   when E_Program_Termination =>
      return To_Exit_Status (E_Fatal);

   when E : others =>
      Handle_Program_Termination
        (Force_Exit => False,
         Exit_Cause => E_Generic,
         Message    => Exception_Information (E));
      return To_Exit_Status (E_Fatal);
end GPRbuild.Main;
