------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
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
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with GNAT.OS_Lib;
with GNATCOLL.OS.Dir;

with GNATCOLL.Traces;
with GNATCOLL.Utils;

with GPR2.Build.Actions_Population;
with GPR2.Build.Actions.Compile;
with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Actions.Link;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Compilation_Unit;
with GPR2.Log;
with GPR2.Message;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Configuration;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.View.Vector;
with GPR2.Reporter;
with GPR2.Source_Reference;

with GPRtools.Options;
with GPRtools.Program_Termination;
with GPRtools.Util;

with GPRclean.Options;

function GPRclean.Main return Ada.Command_Line.Exit_Status is

   use Ada;
   use Ada.Exceptions;

   use GPR2;
   use GPR2.Build;
   use GPRtools;
   use GPRtools.Program_Termination;
   use GPR2.Path_Name;

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   procedure Delete_File
     (Name : String; Opts : GPRclean.Options.Object);
   --  Remove file if exists.
   --  Opts parameter need because the command line options is used inside of
   --  this routine and could be different for different projects because of
   --  Switches attribute in project package Clean.

   procedure Delete_File
     (Dir     : Path_Name.Object;
      Pattern : String;
      Opts    : GPRclean.Options.Object);
   --  Similar to above, but accepts a file pattern and then looks up the
   --  matching files in Dir.

   procedure Remove_Artifacts_Dirs
     (View : GPR2.Project.View.Object; Opts : GPRclean.Options.Object);
   --  Removes the empty obj/lib/exec dirs of View

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File
     (Dir     : Path_Name.Object;
      Pattern : String;
      Opts    : GPRclean.Options.Object)
   is
      DH : GNATCOLL.OS.Dir.Dir_Handle;
      DE : GNATCOLL.OS.Dir.Dir_Entry;
      use GNATCOLL.OS.Dir;

   begin
      if not Dir.Exists then
         return;
      end if;

      DH := GNATCOLL.OS.Dir.Open (Dir.String_Value);

      loop
         DE := GNATCOLL.OS.Dir.Read (DH);

         exit when GNATCOLL.OS.Dir.End_Of_Iteration (DE);

         if Is_File (DE)
           and then GNATCOLL.Utils.Match (Name (DE), Pattern)
         then
            Delete_File
              (Dir.Compose (Filename_Type (Name (DE))).String_Value, Opts);
         end if;
      end loop;

      GNATCOLL.OS.Dir.Close (DH);
   end Delete_File;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File
     (Name : String; Opts : GPRclean.Options.Object)
   is
      use GNAT.OS_Lib;
      use GPR2.Reporter;
      Success : Boolean := False;
   begin
      if Is_Regular_File (Name) then
         if Opts.Dry_Run then
            Text_IO.Put_Line (Name);

         else
            if GNAT.OS_Lib.Is_Owner_Writable_File (Name) then
               Delete_File (Name, Success);

            elsif Opts.Force_Deletions then
               GNAT.OS_Lib.Set_Writable (Name);
               Delete_File (Name, Success);
            end if;

            if Success then
               Opts.Tree.Reporter.Report
                 ('"' & Name & """ has been deleted",
                  Level => GPR2.Message.Optional);
            elsif Opts.Tree.Reporter.Verbosity > No_Warnings then
               Opts.Tree.Reporter.Report
                 ("Warning: """ & Name & """ could not be deleted");
            end if;
         end if;
      end if;
   end Delete_File;

   ---------------------------
   -- Remove_Artifacts_Dirs --
   ---------------------------

   procedure Remove_Artifacts_Dirs
     (View : GPR2.Project.View.Object; Opts : GPRclean.Options.Object)
   is
      procedure Remove_Dir (Path : GPR2.Path_Name.Object);

      Subdirs  : constant Filename_Optional :=
                   Opts.Tree.Subdirs;

      ----------------
      -- Remove_Dir --
      ----------------

      procedure Remove_Dir (Path : GPR2.Path_Name.Object) is
         use Ada.Directories;
      begin
         if Path = View.Dir_Name then
            return;
         end if;

         if not Path.Exists then
            return;
         end if;

         if Kind (Path.String_Value) = Directory then
            Delete_Directory (Path.String_Value);
         end if;

         if Subdirs'Length > 0 then
            if Path.Containing_Directory = View.Dir_Name then
               return;
            end if;

            Delete_Directory (Path.Containing_Directory.String_Value);
         end if;

      exception
         when Use_Error =>
            declare
               Search : Search_Type;
            begin
               Start_Search (Search, Path.String_Value, "");
               Opts.Tree.Reporter.Report
                 ("warning: Directory """ & Path.String_Value
                  & """ could not be removed"
                  & (if More_Entries (Search)
                    then " because it is not empty"
                    else "") & '.');
            end;
      end Remove_Dir;


   begin
      if View.Kind in GPR2.With_Object_Dir_Kind then
         Remove_Dir (View.Object_Directory);

         if View.Is_Namespace_Root
           and then View.Has_Mains
           and then View.Executable_Directory /= View.Object_Directory
         then
            Remove_Dir (View.Executable_Directory);
         end if;
      end if;

      if View.Is_Library then
         Remove_Dir (View.Library_Directory);

         if View.Library_Ali_Directory /= View.Library_Directory then
            Remove_Dir (View.Library_Ali_Directory);
         end if;
      end if;
   end Remove_Artifacts_Dirs;

   Opt           : GPRclean.Options.Object;
   Parser        : GPRtools.Options.Command_Line_Parser;
   Lang          : GPR2.Language_Id;
   Artifact_Path : Path_Name.Object;

   use type GPR2.Project.View.Object;

begin
   GNATCOLL.Traces.Parse_Config_File;
   GPRtools.Util.Set_Program_Name ("gprclean");
   GPRclean.Options.Setup (Parser);
   GPRclean.Options.Parse_Command_Line (Parser, Opt);

   if not GPRtools.Options.Load_Project (Opt, GPR2.No_Error) then
      Handle_Program_Termination (Message => "");
   end if;

   --  Check gprclean's Switch attribute from loaded project

   declare
      Attr : constant GPR2.Project.Attribute.Object :=
               Opt.Tree.Root_Project.Attribute (PRA.Clean.Switches);
      Tree : constant GPR2.Project.Tree.Object := Opt.Tree;
   begin
      if Attr.Is_Defined then
         Opt := (GPRtools.Options.Empty_Options
                 with others => <>);
         Opt.Tree := Tree;

         GPRclean.Options.Parse_Attribute_Switches
           (Parser, Opt, Attr.Values);

         --  re-parse the command line to allow it to overwrite project
         --  defined Switches attribute.

         GPRclean.Options.Parse_Command_Line (Parser, Opt);

         --  Set the reporter in case it has changed in the new options
         Opt.Tree.Set_Reporter (Opt.Console_Reporter);
      end if;
   end;

   if Opt.Tree.Has_Configuration
     and then Opt.Tree.Configuration.Log_Messages.Has_Element
       (Error    => True,
        Warning  => True,
        Hint     => False,
        End_User => False)
   then
      Opt.Tree.Reporter.Report
        (GPR2.Message.Create
           (GPR2.Message.Warning,
            "Cleaning may be incomplete, as there were problems during"
            & " auto-configuration",
            Source_Reference.Create
              (Opt.Tree.Root_Project.Path_Name.Value, 0, 0)));
   end if;

   Opt.Tree.Update_Sources;

   --  Create actions that will be used to iterate and obtain artifacts
   --  for removal.

   if not GPR2.Build.Actions_Population.Populate_Actions
     (Opt.Tree, Opt.Build_Options)
   then
      return To_Exit_Status (E_Abort);
   end if;

   --  Iterate on all actions, and clean their output artifacts

   for Action of Opt.Tree.Artifacts_Database.All_Actions loop
      if not Action.View.Is_Externally_Built
        and then (Opt.All_Projects
                  or else Action.View = Opt.Tree.Root_Project)
        and then not
          (Opt.Compil_Only
           and then Action in GPR2.Build.Actions.Link.Object'Class)
      then
         for Artifact of
           Opt.Tree.Artifacts_Database.Outputs (Action.UID)
         loop
            if Artifact in GPR2.Build.Artifacts.Files.Object'Class then
               Artifact_Path :=
                 GPR2.Build.Artifacts.Files.Object'Class (Artifact).Path;

               Delete_File (Artifact_Path.String_Value, Opt);

               if Action in GPR2.Build.Actions.Compile.Object'Class then
                  Lang :=
                    GPR2.Build.Actions.Compile.Object'Class (Action).Language;
                  declare
                     Src_Exts : constant GPR2.Project.Attribute.Object :=
                                  Action.View.Attribute
                                    (PRA.Clean.Source_Artifact_Extensions,
                                     PAI.Create (Lang));
                     Obj_Exts : constant GPR2.Project.Attribute.Object :=
                                  Action.View.Attribute
                                    (PRA.Clean.Object_Artifact_Extensions,
                                     PAI.Create (Lang));
                     Obj_BN   : constant Filename_Type :=
                                  Artifact_Path.Base_Filename;
                     Obj_Dir  : constant Path_Name.Object :=
                                  Artifact_Path.Containing_Directory;
                     Path     : Path_Name.Object;
                  begin
                     if Obj_Exts.Is_Defined then
                        for Val of Obj_Exts.Values loop
                           Path := Obj_Dir.Compose
                             (Obj_BN &
                              Filename_Type
                                (if Val.Text (Val.Text'First) = '.'
                                 then Val.Text
                                 else "." & Val.Text));
                           Delete_File (Path.String_Value, Opt);
                        end loop;
                     end if;

                     if Src_Exts.Is_Defined then
                        for Val of Src_Exts.Values loop
                           if Action in Actions.Compile.Ada.Object'Class then
                              declare
                                 CU : constant Compilation_Unit.Object :=
                                        Actions.Compile.Ada.Object'Class
                                          (Action).Input_Unit;
                                 procedure For_Part
                                   (Kind     : Unit_Kind;
                                    View     : GPR2.Project.View.Object;
                                    Path     : Path_Name.Object;
                                    Index    : Unit_Index;
                                    Sep_Name : Optional_Name_Type);

                                 procedure For_Part
                                   (Kind     : Unit_Kind;
                                    View     : GPR2.Project.View.Object;
                                    Path     : Path_Name.Object;
                                    Index    : Unit_Index;
                                    Sep_Name : Optional_Name_Type)
                                 is
                                    Candidate : Path_Name.Object;
                                    pragma Unreferenced
                                      (Kind, View, Index, Sep_Name);
                                 begin
                                    Candidate := Obj_Dir.Compose
                                      (Path.Simple_Name &
                                       Filename_Type
                                         (if Val.Text (Val.Text'First) = '.'
                                          then Val.Text
                                          else "." & Val.Text));
                                    Delete_File (Candidate.String_Value, Opt);
                                 end For_Part;
                              begin
                                 CU.For_All_Part (For_Part'Access);
                              end;
                           end if;

                           Path := Obj_Dir.Compose
                             (Obj_BN &
                              Filename_Type
                                (if Val.Text (Val.Text'First) = '.'
                                 then Val.Text
                                 else "." & Val.Text));
                           Delete_File (Path.String_Value, Opt);
                        end loop;
                     end if;
                  end;
               end if;
            end if;
         end loop;

         Delete_File
           (Action.View.Object_Directory.Compose
              (GPR2.Build.Actions.Db_Filename (Action.UID)).String_Value,
            Opt);
      end if;
   end loop;

   if Opt.Remove_Config then
      Delete_File (Opt.Config_Project.String_Value, Opt);
   end if;

   declare
      Views    : GPR2.Project.View.Vector.Object;
      Obj_Attr : constant GPR2.Project.Attribute.Object :=
                   Opt.Tree.Root_Project.Attribute
                     (PRA.Clean.Artifacts_In_Object_Dir);
      Exe_Attr : constant GPR2.Project.Attribute.Object :=
                   Opt.Tree.Root_Project.Attribute
                     (PRA.Clean.Artifacts_In_Exec_Dir);
   begin
      --  Cleanup in object/exec dirs

      if Opt.All_Projects then
         Views := Opt.Tree.Ordered_Views;
      else
         Views.Append (Opt.Tree.Root_Project);
      end if;

      for V of Views loop
         if not V.Is_Externally_Built then
            if Obj_Attr.Is_Defined then
               if V.Kind in With_Object_Dir_Kind then
                  for Val of Obj_Attr.Values loop
                     Delete_File
                       (V.Object_Directory,
                        Val.Text,
                        Opt);
                  end loop;
               else
                  for Val of Obj_Attr.Values loop
                     Delete_File
                       (V.Dir_Name,
                        Val.Text,
                        Opt);
                  end loop;
               end if;
            end if;

            if Exe_Attr.Is_Defined
              and then V.Kind = K_Standard
            then
               for Val of Exe_Attr.Values loop
                  Delete_File
                    (V.Executable_Directory,
                     Val.Text,
                     Opt);
               end loop;
            end if;

            if Opt.Remove_Empty_Dirs then
               Remove_Artifacts_Dirs (V, Opt);
            end if;
         end if;
      end loop;
   end;

   return To_Exit_Status (E_Success);

exception
   when E : GPR2.Options.Usage_Error =>
      Handle_Program_Termination
        (Display_Command_Line_Help => True,
         Force_Exit                => False,
         Message                   => Exception_Message (E));
      return To_Exit_Status (E_Fatal);

   when Project_Error  =>
   Handle_Program_Termination
     (Force_Exit => False,
      Message    =>
        '"' & String (Opt.Project_File.Simple_Name) &
        """ processing failed");
      return To_Exit_Status (E_Fatal);

   when E_Program_Termination =>
      return To_Exit_Status (E_Fatal);

   when E : others =>
      Handle_Program_Termination
        (Force_Exit => False,
         Exit_Cause => E_Generic,
         Message    => Exception_Message (E));
      return To_Exit_Status (E_Fatal);
end GPRclean.Main;
