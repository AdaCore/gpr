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

with GNATCOLL.Traces;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Process_Manager.JSON;
with GPR2.Build.Source;
with GPR2.Interrupt_Handler;
with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;

with GPRtools.Options;
with GPRtools.Program_Termination;
with GPRtools.Sigint;
with GPRtools.Util;
with GPRtools.Actions;

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

   function Ensure_Directories
     (Tree : GPR2.Project.Tree.Object) return Boolean;

   Opt       : Options.Object;

   ------------------------
   -- Ensure_Directories --
   ------------------------

   function Ensure_Directories
     (Tree : GPR2.Project.Tree.Object) return Boolean
   is
      procedure Ensure (Path : GPR2.Path_Name.Object);
      --  Make sure Path exists and is a directory.

      procedure Mkdir_Recursive (Path : GPR2.Path_Name.Object);
      --  Creates Path recursively

      All_Ok : Boolean := True;

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
            if Opt.Create_Missing_Dirs then
               Mkdir_Recursive (Path);
               Tree.Reporter.Report ('"' & Path_Img & """ created");
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

      procedure Mkdir_Recursive (Path : GPR2.Path_Name.Object) is
         Parent : constant GPR2.Path_Name.Object :=
                    Path.Containing_Directory;
      begin
         if not Parent.Exists then
            Mkdir_Recursive (Parent);
         end if;

         Ada.Directories.Create_Directory (Path.String_Value);
      end Mkdir_Recursive;

   begin
      for V of Tree.Ordered_Views loop
         if not V.Is_Externally_Built then
            if V.Kind in GPR2.With_Object_Dir_Kind then
               Ensure (V.Object_Directory);
            end if;

            if V.Is_Library then
               Ensure (V.Library_Directory);
               Ensure (V.Library_Ali_Directory);
            end if;
         end if;
      end loop;

      for V of Tree.Namespace_Root_Projects loop
         if V.Has_Mains then
            Ensure (V.Executable_Directory);
         end if;
      end loop;

      return All_Ok;
   end Ensure_Directories;

   Parser    : constant Options.GPRBuild_Parser := Options.Create;
   Tree      : Project.Tree.Object;
   Sw_Attr   : GPR2.Project.Attribute.Object;
   Messages  : GPR2.Log.Object;
   Process_M : GPR2.Build.Process_Manager.JSON.Object;
   Jobs_JSON : GPR2.Path_Name.Object;

begin

   GNATCOLL.Traces.Parse_Config_File;

   --  Install the Ctrl-C handler

   GPR2.Interrupt_Handler.Install_Sigint (GPRtools.Sigint.Handler'Access);

   --  Set program name

   GPRtools.Util.Set_Program_Name ("gprbuild");

   --  Parse arguments

   Parser.Get_Opt (Opt);

   --  Load the project tree

   if not GPRtools.Options.Load_Project (Opt, GPR2.No_Error) then
      Handle_Program_Termination (Message => "");
   end if;

   Tree := Opt.Tree;

   if not Tree.Update_Sources then
      Handle_Program_Termination
        (Force_Exit => True,
         Exit_Cause => E_Tool,
         Message    => "Failed to update sources");
      return To_Exit_Status (E_Fatal);
   end if;

   --  Check if we have a Builder'Switches attribute in the root project

   if Tree.Root_Project.Has_Package (PRP.Builder) then
      declare
         Mains : constant GPR2.Build.Compilation_Unit.Unit_Location_Vector :=
                   (if Opt.Mains.Is_Empty
                    then Tree.Root_Project.Mains
                    else Opt.Mains);
      begin
         --  #1: If one main is defined, from the Main top-level attribute or
         --  from the command line, we fetch Builder'Switches(<main>).

         if Mains.Length = 1 then
            declare
               Source_Part :
                 constant GPR2.Build.Compilation_Unit.Unit_Location :=
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
      end;

      --  #4 check Switches (others)

      if not Sw_Attr.Is_Defined then
         Sw_Attr := Tree.Root_Project.Attribute
           (Name  => PRA.Builder.Switches,
            Index => Project.Attribute_Index.I_Others);
      end if;

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

   if not Tree.Update_Sources (Option => Sources_Units_Artifacts) then
      Handle_Program_Termination
        (Force_Exit => True,
         Exit_Cause => E_Tool,
         Message    => "Failed to update sources");
      return To_Exit_Status (E_Fatal);
   end if;

   if not Ensure_Directories (Tree) then
      Handle_Program_Termination (Force_Exit => False);

      return To_Exit_Status (E_Abort);
   end if;

   Messages.Clear;

   if not GPRtools.Actions.Add_Actions_To_Build_Mains (Tree) then
      return To_Exit_Status (E_Abort);
   end if;

   if Opt.Json_Summary then
      if Tree.Root_Project.Kind in With_Object_Dir_Kind then
         Jobs_JSON := Tree.Root_Project.Object_Directory.Compose ("jobs.json");
      else
         Jobs_JSON := Tree.Root_Project.Dir_Name.Compose ("jobs.json");
      end if;

      Process_M.Execute
        (Tree.Artifacts_Database,
         Jobs            => Opt.Parallel_Tasks,
         JSON_File       => Jobs_JSON,
         Stop_On_Fail    => not Opt.Keep_Going,
         Keep_Temp_Files => Opt.Keep_Temp_Files);
   else
      GPR2.Build.Process_Manager.Object (Process_M).Execute
        (Tree.Artifacts_Database,
         Jobs            => Opt.Parallel_Tasks,
         Stop_On_Fail    => not Opt.Keep_Going,
         Keep_Temp_Files => Opt.Keep_Temp_Files);
   end if;

   return To_Exit_Status (E_Success);

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
