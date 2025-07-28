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

with Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Traces;

with GPR2.Build.Actions_Population;
with GPR2.Build.Compilation_Unit;
with GPR2.Build.External_Options;
with GPR2.Build.Process_Manager.JSON;
with GPR2.Build.Source;
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

with GPRtools.Interrupt_Handler;
with GPRtools.Options;
with GPRtools.Program_Termination;
with GPRtools.Sigint;
with GPRtools.Util;

with GPRbuild.Options;

-------------------
-- GPRbuild.Main --
-------------------

procedure GPRbuild.Main is

   use Ada;
   use type Ada.Containers.Count_Type;
   use Ada.Exceptions;

   use GPR2;
   use GPRtools;
   use GPRtools.Program_Termination;

   package PRP renames GPR2.Project.Registry.Pack;
   package PRA renames GPR2.Project.Registry.Attribute;

   function Execute
     (PM : in out GPR2.Build.Process_Manager.Object'Class)
      return Exit_Code_Type;

   Opt  : Options.Object;

   -------------
   -- Execute --
   -------------

   function Execute
     (PM : in out GPR2.Build.Process_Manager.Object'Class)
      return Exit_Code_Type is
   begin
      case Opt.Tree.Artifacts_Database.Execute (PM, Opt.PM_Options) is
         when GPR2.Build.Process_Manager.Success =>
            return E_Success;
         when GPR2.Build.Process_Manager.Errors =>
            return E_Errors;
         when GPR2.Build.Process_Manager.Failed =>
            return E_Fatal;
      end case;
   end Execute;

   Parser         : constant Options.GPRbuild_Parser := Options.Create;
   Sw_Attr        : GPR2.Project.Attribute.Object;
   Process_M      : GPR2.Build.Process_Manager.Object;
   Process_M_JSON : GPR2.Build.Process_Manager.JSON.Object;
   Jobs_JSON      : GPR2.Path_Name.Object;

   use GPR2.Build;

begin

   GNATCOLL.Traces.Parse_Config_File;

   --  Install the Ctrl-C handler

   GPRtools.Interrupt_Handler.Install_Sigint (GPRtools.Sigint.Handler'Access);

   --  Set program name

   GPRtools.Util.Set_Program_Name ("gprbuild");

   --  Parse arguments

   Parser.Get_Opt (Opt);

   --  Load the project tree and sources

   if not GPRtools.Options.Load_Project
     (Opt,
      Absent_Dir_Error        => GPR2.Error,
      Create_Missing_Dirs     => (if Opt.Create_Missing_Dirs
                                  then GPR2.Project.Tree.Create_Always
                                  else GPR2.Project.Tree.Create_Relative),
      Handle_Errors           => True,
      Restricted_To_Languages => Opt.Build_Options.Restricted_To_Languages)
   then
      return;
   end if;

   --  Check if there's something to do

   if Opt.Build_Options.Mains.Is_Empty then
      declare
         Is_Empty : Boolean := True;
      begin
         NS_Loop :
         for NS of Opt.Tree.Namespace_Root_Projects loop
            if NS.Kind /= K_Abstract then
               Is_Empty := False;
               exit NS_Loop;
            end if;

            for V of NS.Closure (False, False, True) loop
               if not V.Is_Externally_Built then
                  Is_Empty := False;
                  exit NS_Loop;
               end if;
            end loop;
         end loop NS_Loop;

         if Is_Empty then
            if Opt.Tree.Log_Messages.Has_Element
              (Error    => True,
               End_User => False,
               Warning  => True,
               Hint     => False,
               Lint     => False,
               Read     => True,
               Unread   => True)
            then
               Handle_Program_Termination
                 (Force_Exit => False,
                  Exit_Code  => E_Project,
                  Exit_Cause => E_Tool,
                  Message    => "no sources to compile");
            else
               Opt.Tree.Reporter.Report ("gprbuild: no sources to compile");
            end if;

            return;
         end if;
      end;
   end if;

   --  Load the sources

   if not Opt.Tree.Update_Sources (Option => Sources_Units_Artifacts) then
      Handle_Program_Termination
        (Force_Exit => True,
         Exit_Code  => E_Project,
         Exit_Cause => E_Tool,
         Message    => "processing failed");
   end if;

   --  Check if we have a Builder'Switches attribute in the root project

   if Opt.Tree.Root_Project.Has_Package (PRP.Builder)
     and then not Opt.Tree.Root_Project.Attributes
       (PRA.Builder.Switches).Is_Empty
   then
      declare
         Has_Error : Boolean := False;
         Mains : constant Compilation_Unit.Unit_Location_Vector :=
                   (if not Opt.Build_Options.Mains.Is_Empty
                    then Actions_Population.Resolve_Mains
                      (Opt.Tree, Opt.Build_Options, Has_Error)
                    elsif Opt.Tree.Root_Project.Has_Mains
                    then Opt.Tree.Root_Project.Mains
                    else GPR2.Build.Compilation_Unit.Empty_Vector);
      begin
         if Has_Error then
            Handle_Program_Termination
              (Force_Exit => True,
               Exit_Code  => E_Project,
               Exit_Cause => E_Tool,
               Message    => "processing failed");
         end if;

         --  #1: If one main is defined, from the Main top-level attribute or
         --  from the command line, we fetch Builder'Switches(<main>).

         if Mains.Length = 1 then
            declare
               Source_Part : constant Compilation_Unit.Unit_Location :=
                               Mains.First_Element;
            begin
               Sw_Attr := Opt.Tree.Root_Project.Attribute
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
                  Src :=
                    Opt.Tree.Root_Project.Source (Main.Source.Simple_Name);

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
                  Sw_Attr := Opt.Tree.Root_Project.Attribute
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
               for Val of Opt.Tree.Root_Project.Languages loop
                  New_Lang := +Optional_Name_Type (Val.Text);

                  Driver_Attr := Opt.Tree.Root_Project.Attribute
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
                  Sw_Attr := Opt.Tree.Root_Project.Attribute
                    (Name  => PRA.Builder.Switches,
                     Index => Project.Attribute_Index.Create (Lang));
               end if;
            end;
         end if;

         --  #4 check Switches (others)

         if not Sw_Attr.Is_Defined then
            Sw_Attr := Opt.Tree.Root_Project.Attribute
              (Name  => PRA.Builder.Switches,
               Index => Project.Attribute_Index.I_Others);
         end if;

         if not Sw_Attr.Is_Defined
           and then Mains.Length > 1
         then
            Opt.Tree.Reporter.Report
              ("warning: Builder'Switches attribute is ignored as there are" &
                 " several mains");
         end if;
      end;

      --  Finally, if we found a Switches attribute, apply it

      if Sw_Attr.Is_Defined then
         declare
            --  Preserve the Tree object
            Tree : constant GPR2.Project.Tree.Object := Opt.Tree;
         begin
            Opt := Options.Object'(GPRtools.Options.Empty_Options
                                   with others => <>);
            Opt.Tree := Tree;

            Parser.Get_Opt (From_Pack => PRP.Builder,
                            Values    => Sw_Attr.Values,
                            Result    => Opt);
            Parser.Get_Opt (Opt);

            --  Manually update the tree reporter in case the verbosity has
            --  been changed in Opt.
            Opt.Tree.Set_Reporter (Opt.Console_Reporter);
         end;
      end if;
   end if;

   --  Handle Builder'Global_Compilation_Switches

   if Opt.Tree.Root_Project.Has_Package (PRP.Builder)
     and then not Opt.Tree.Root_Project.Attributes
       (PRA.Builder.Global_Compilation_Switches).Is_Empty
   then
      for Attr of Opt.Tree.Root_Project.Attributes
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

   if Opt.Tree.Root_Project.Is_Library
     and then Opt.Tree.Root_Project.Is_Library_Standalone
     and then not Opt.Extra_Args.Fetch (GPR2.Build.External_Options.Binder,
                                        Ada_Language).Is_Empty
   then
      Opt.Console_Reporter.Report
        (GPR2.Message.Create
           (GPR2.Message.Warning,
            "binding options on the command line are not taken into account" &
              " when the main project is a Stand-Alone Library project",
            GPR2.Source_Reference.Create
              (Opt.Tree.Root_Project.Path_Name.Value, 0, 0)));
      Opt.Extra_Args.Clear (GPR2.Build.External_Options.Binder);
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
               Exit_Code  => E_Errors,
               Exit_Cause => E_Tool,
               Message    => "processing failed");
         end if;
      end;
   end if;

   --  Set user-specified cargs/bargs/largs if any

   Opt.Tree.Artifacts_Database.Set_External_Options (Opt.Extra_Args);

   --  Now populate the Build database's actions

   if not GPR2.Build.Actions_Population.Populate_Actions
     (Opt.Tree, Opt.Build_Options, Static_Actions => False)
   then
      Handle_Program_Termination
        (Force_Exit => True,
         Exit_Code  => E_Project,
         Exit_Cause => E_Tool,
         Message    => "processing failed");
   end if;

   declare
      Result : Exit_Code_Type;
   begin
      if Opt.Json_Summary then
         Jobs_JSON := Opt.Tree.Root_Project.Dir_Name.Compose ("jobs.json");
         Process_M_JSON.Set_JSON_File (Jobs_JSON);

         Result := Execute (Process_M_JSON);
      else
         Result := Execute (Process_M);
      end if;

      if Result /= E_Success then
         --  GPRbuild silently fails with a error code, which makes it
         --  not obvious at first glance something went wrong.
         Handle_Program_Termination
           (Exit_Code => Result,
            Message   => "process manager execution failed");
      end if;
   end;

exception
   when E : GPR2.Options.Usage_Error =>
      Handle_Program_Termination
        (Display_Command_Line_Help => True,
         Force_Exit                => False,
         Exit_Code                 => E_General,
         Message                   => Exception_Message (E));

   when E_Program_Termination =>
      --  Exit code already positioned and messages are displayed: nothing
      --  more to do here.
      null;

   when E : others =>
      Handle_Program_Termination
        (Force_Exit => False,
         Exit_Code  => E_Fatal,
         Exit_Cause => E_Generic,
         Message    => Exception_Information (E));
end GPRbuild.Main;
