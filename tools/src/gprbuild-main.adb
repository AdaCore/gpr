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
with Ada.Exceptions;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Process_Manager.JSON;
with GPR2.Build.Source;
with GPR2.Interrupt_Handler;
with GPR2.Log;
with GPR2.Options;
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

with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Traces;

-------------------
-- GPRbuild.Main --
-------------------

function GPRbuild.Main return Ada.Command_Line.Exit_Status is

   use Ada;
   use type Ada.Containers.Count_Type;
   use Ada.Exceptions;

   use GPR2;

   use GPRtools.Program_Termination;

   package PRP renames GPR2.Project.Registry.Pack;
   package PRA renames GPR2.Project.Registry.Attribute;

   Parser    : constant Options.GPRBuild_Parser := Options.Create;
   Opt       : Options.Object;
   Tree      : Project.Tree.Object;
   Sw_Attr   : GPR2.Project.Attribute.Object;
   Messages  : GPR2.Log.Object;
   Process_M : GPR2.Build.Process_Manager.JSON.Object;

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
      Handle_Program_Termination
        (Opt     => Opt,
         Message => "");
   end if;

   Tree := Opt.Tree;
   Tree.Update_Sources (Messages => Messages);

   if Opt.Verbose then
      if Tree.Has_Messages then
         Tree.Log_Messages.Output_Messages;
      end if;
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

   Tree.Update_Sources
     (Option   => Sources_Units_Artifacts,
      Messages => Messages);

   if Opt.Verbose then
      if Tree.Has_Messages then
         Tree.Log_Messages.Output_Messages;
      end if;
   end if;

   Make_Dir
     (GNATCOLL.VFS.Create
        (Filesystem_String (Tree.Root_Project.Object_Directory.Value)));

   Messages.Clear;
   if Tree.Root_Project.Is_Library then

      --  Create actions to build a lib

      null;
   else
      if not GPRtools.Actions.Add_Actions_To_Build_Mains (Tree, Messages) then
         Messages.Output_Messages
           (Information => True, Warning => True, Error => True);
         return To_Exit_Status (E_Abort);
      end if;
   end if;

   Change_Dir
     (GNATCOLL.VFS.Create
       (Filesystem_String (Tree.Root_Project.Object_Directory.Value)));

   Process_M.Execute (Tree.Artifacts_Database, 1);

   return To_Exit_Status (E_Success);

exception
   when E : GPR2.Options.Usage_Error =>
      Handle_Program_Termination
        (Opt                       => Opt,
         Display_Command_Line_Help => True,
         Force_Exit                => False,
         Message                   => Exception_Message (E));
      return To_Exit_Status (E_Fatal);

   when E_Program_Termination =>
      return To_Exit_Status (E_Fatal);

   when E : others =>
      Handle_Program_Termination
        (Opt        => Opt,
         Force_Exit => False,
         Exit_Cause => E_Generic,
         Message    => Exception_Message (E));
      return To_Exit_Status (E_Fatal);
end GPRbuild.Main;
