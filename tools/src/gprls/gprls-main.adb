------------------------------------------------------------------------------
--                                                                          --
--                           GPRLS                                          --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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

with Ada.Text_IO;

with GNATCOLL.Traces;
with Ada.Strings.Unbounded;
with GPR2;
with GPR2.Build.Actions;
with GPR2.Build.Actions_Population;
with GPR2.Build.Actions.Compile;
with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Artifacts;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.Source_Files;
with GPR2.Build.Compilation_Unit;
with GPR2.Build.Signature;
with GPR2.Build.Tree_Db;
with GPR2.Log;
with GPR2.Message;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Configuration;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Reporter;
with GPR2.Source_Reference;

with GPRtools.Options;
with GPRtools.Program_Termination;
with GPRtools.Util;

with GPRls.Options;

function GPRls.Main return Ada.Command_Line.Exit_Status is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("GPRLS", GNATCOLL.Traces.Off);

   use Ada.Exceptions;

   use GPR2;
   use GPRtools.Program_Termination;
   use GPR2.Path_Name;

   Opt    : GPRls.Options.Object;
   Parser : GPRtools.Options.Command_Line_Parser;

   procedure Print_Line (Str : String; Offset : Integer := 0);
   --  Print a line with an optional offset (for indentation)

   procedure Print_Artifact
     (Artifact    : GPR2.Build.Artifacts.Object'Class;
      Signature   : GPR2.Build.Signature.Object;
      With_Status : Boolean;
      Offset      : Integer := 0;
      Simple_Name : Boolean := False);
   --  Print the artifact.
   --  @param Artifact The artifact to print.
   --  @param Signature The signature to use to check the artifact status if
   --     the status needs to be printed.
   --  @param With_Status If True, print the status (OK or DIF) of the
   --     artifact.
   --  @param Offset The Print_Line offset to use when printing the artifact.
   --  @param Simple_Name If True, print only the simple name of the artifact
   --    instead of the full path.

   procedure Print_Action
     (Action           : GPR2.Build.Actions.Compile.Object'Class;
      Runtime_Deps     : GPR2.Path_Name.Set.Object;
      Non_Runtime_Deps : GPR2.Path_Name.Set.Object;
      Signature        : GPR2.Build.Signature.Object);
   --  Print the action according to the options in Opt.
   --  @param Action The action to print.
   --  @param Runtime_Deps The set of runtime dependencies of the action.
   --  @param Non_Runtime_Deps The set of non-runtime dependencies of the
   --     action.
   --  @param Signature The signature to use to check the action status if
   --     the status needs to be printed.

   function Unit_Name_To_Unit
     (Tree : GPR2.Project.Tree.Object; Unit_Name : Name_Type)
      return GPR2.Build.Compilation_Unit.Object;
   --  Return the compilation unit corresponding to the unit name in the given
   --  project tree.
   --  If not found, returns an undefined compilation unit.

   function Unit_Name_To_Ada_Compile_Action
     (Tree : GPR2.Project.Tree.Object; Unit_Name : Name_Type)
      return GPR2.Build.Actions.Compile.Ada.Object;
   --  Return the Ada compile action corresponding to the unit name in the
   --  given project tree.
   --  If not found, returns an undefined action.

   function Source_To_Ada_Compile_Action
     (Tree : GPR2.Project.Tree.Object; Source_Name : Filename_Type)
      return GPR2.Build.Actions.Compile.Ada.Object;
   --  Return the Ada compile action corresponding to the source file name in
   --  the given project tree.
   --  If not found, returns an undefined action.

   function Compute_Closure
     (Tree : GPR2.Project.Tree.Object; Source_Name : Filename_Type)
      return GPR2.Build.Tree_Db.Action_Sets.Set;
   --  Compute all the actions required to compile the given source file in
   --  the given project tree.

   ---------------------
   -- Compute_Closure --
   ---------------------

   function Compute_Closure
     (Tree : GPR2.Project.Tree.Object; Source_Name : Filename_Type)
      return GPR2.Build.Tree_Db.Action_Sets.Set
   is
      use GPR2.Build.Tree_Db.Action_Sets;

      Closure     : GPR2.Build.Tree_Db.Action_Sets.Set;
      To_Process  : GPR2.Build.Tree_Db.Action_Sets.Set;
      Inserted    : Boolean := False;
      Unused_Curs : GPR2.Build.Tree_Db.Action_Sets.Cursor;
      Comp_Action : GPR2.Build.Actions.Compile.Ada.Object :=
        Source_To_Ada_Compile_Action (Tree, Source_Name);
   begin
      Traces.Trace ("Computing Closure for " & String (Source_Name) & ":");
      if Comp_Action.Is_Defined then
         Include (To_Process, Comp_Action.UID);
      end if;

      while not To_Process.Is_Empty loop
         declare
            Action_UID : constant GPR2.Build.Actions.Action_Id'Class :=
              To_Process.First_Element;
            Action     : constant GPR2.Build.Actions.Object'Class :=
              Opt.Tree.Artifacts_Database.Action_Id_To_Reference (Action_UID);
         begin
            if Action in GPR2.Build.Actions.Compile.Ada.Object'Class then
               Comp_Action := GPR2.Build.Actions.Compile.Ada.Object (Action);
            end if;
         end;

         To_Process.Delete_First;

         if Comp_Action.Is_Defined then
            Insert (Closure, Comp_Action.UID, Unused_Curs, Inserted);
            Traces.Trace ("* " & Comp_Action.UID.Image);

            --  Do not process twice the action if it was already in
            --  the closure

            if Inserted then
               for Withed_Unit of Comp_Action.Withed_Units loop
                  Traces.Trace ("  - with " & String (Withed_Unit));
                  Comp_Action :=
                    Unit_Name_To_Ada_Compile_Action (Tree, Withed_Unit);
                  if Comp_Action.Is_Defined then
                     Include (To_Process, Comp_Action.UID);
                  end if;
               end loop;
            end if;
         end if;
      end loop;

      return Closure;
   end Compute_Closure;

   ------------------
   -- Print_Action --
   ------------------

   procedure Print_Action
     (Action           : GPR2.Build.Actions.Compile.Object'Class;
      Runtime_Deps     : GPR2.Path_Name.Set.Object;
      Non_Runtime_Deps : GPR2.Path_Name.Set.Object;
      Signature        : GPR2.Build.Signature.Object)
   is
      Offset : Integer := 0;

   begin
      if Opt.Print_Object_Files then
         Print_Artifact
           (Action.Object_File, Signature, not Opt.Hide_Status, Offset);
         Offset := Offset + 1;
      end if;

      if Opt.Print_Units then
         if Action in GPR2.Build.Actions.Compile.Ada.Object'Class then
            declare
               Compile_Action :
                 constant GPR2.Build.Actions.Compile.Ada.Object :=
                   GPR2.Build.Actions.Compile.Ada.Object (Action);
            begin
               Print_Line (String (Compile_Action.Unit.Name), Offset);
               Offset := Offset + 1;
            end;
         end if;
      end if;

      if Opt.Print_Sources then
         for Artifact of Opt.Tree.Artifacts_Database.Inputs (Action.UID) loop
            if Artifact in GPR2.Build.Artifacts.Source_Files.Object'Class then
               Print_Artifact
                 (Artifact, Signature, not Opt.Hide_Status, Offset);
            end if;
         end loop;
      end if;

      if Opt.Dependency_Mode and then Opt.Print_Sources then
         if not Non_Runtime_Deps.Is_Empty then
            for Dep of Non_Runtime_Deps loop
               Print_Artifact
                 (GPR2.Build.Artifacts.Source_Files.Create (Dep),
                  Signature,
                  not Opt.Hide_Status,
                  Offset);
            end loop;
         end if;
      end if;

      if Opt.With_Predefined_Units and then Opt.Print_Sources then
         if not Runtime_Deps.Is_Empty then
            for Dep of Runtime_Deps loop
               Print_Artifact
                 (GPR2.Build.Artifacts.Source_Files.Create (Dep),
                  Signature,
                  not Opt.Hide_Status,
                  Offset,
                  Opt.Hide_Runtime_Directory);
            end loop;
         end if;
      end if;
   end Print_Action;

   --------------------
   -- Print_Artifact --
   --------------------

   procedure Print_Artifact
     (Artifact    : GPR2.Build.Artifacts.Object'Class;
      Signature   : GPR2.Build.Signature.Object;
      With_Status : Boolean;
      Offset      : Integer := 0;
      Simple_Name : Boolean := False)
   is
      use Ada.Strings.Unbounded;
      Status       : Unbounded_String;
      Artifact_Img : Unbounded_String;
   begin
      if With_Status then
         Status :=
           To_Unbounded_String
             (if Signature.Valid (Artifact) then "OK " else "DIF ");
      end if;

      if Artifact in GPR2.Build.Artifacts.Files.Object'Class then
         if Simple_Name then
            Artifact_Img :=
              To_Unbounded_String
                (String
                   (GPR2.Build.Artifacts.Files.Object (Artifact)
                      .Path
                      .Simple_Name));
         else
            Artifact_Img :=
              To_Unbounded_String
                (GPR2.Build.Artifacts.Files.Object (Artifact)
                   .Path
                   .String_Value);
         end if;
      else
         Artifact_Img := To_Unbounded_String (Artifact.Image);
      end if;

      Print_Line (To_String (Status) & To_String (Artifact_Img), Offset);
   end Print_Artifact;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Str : String; Offset : Integer := 0) is
      Spaces : constant String (1 .. Offset * 3) := (others => ' ');
   begin
      Ada.Text_IO.Put_Line (Spaces & Str);
   end Print_Line;

   ----------------------------------
   -- Source_To_Ada_Compile_Action --
   ----------------------------------

   function Source_To_Ada_Compile_Action
     (Tree : GPR2.Project.Tree.Object; Source_Name : Filename_Type)
      return GPR2.Build.Actions.Compile.Ada.Object
   is
      use GPR2.Build.Actions.Compile.Ada;
      Source_Path : constant GPR2.Path_Name.Object :=
        GPR2.Path_Name.Create_File (Source_Name);
   begin
      Traces.Trace
        ("Searching the Ada compile action for source "
         & String (Source_Name));

      for Action of Tree.Artifacts_Database.All_Actions loop
         if Action in GPR2.Build.Actions.Compile.Ada.Object'Class then
            declare
               Act : constant GPR2.Build.Actions.Compile.Ada.Object :=
                 GPR2.Build.Actions.Compile.Ada.Object (Action);
            begin
               for Input of Tree.Artifacts_Database.Inputs (Act.UID) loop
                  if Input in GPR2.Build.Artifacts.Source_Files.Object'Class
                    and then (GPR2.Build.Artifacts.Source_Files.Object'Class
                                (Input)
                                .Path
                                .Simple_Name
                              = Source_Path.Simple_Name
                              or else GPR2
                                        .Build
                                        .Artifacts
                                        .Source_Files
                                        .Object'Class (Input)
                                        .Path
                                      = Source_Path)
                  then
                     Traces.Trace ("Found compile action: " & Act.UID.Image);
                     return Act;
                  end if;
               end loop;
            end;
         end if;
      end loop;

      Traces.Trace
        ("Did not find any Ada compile action related to "
         & String (Source_Name));
      return GPR2.Build.Actions.Compile.Ada.Undefined;
   end Source_To_Ada_Compile_Action;

   -------------------------------------
   -- Unit_Name_To_Ada_Compile_Action --
   -------------------------------------

   function Unit_Name_To_Ada_Compile_Action
     (Tree : GPR2.Project.Tree.Object; Unit_Name : Name_Type)
      return GPR2.Build.Actions.Compile.Ada.Object
   is
      use GPR2.Build.Actions.Compile.Ada;
      use GPR2.Build.Compilation_Unit;
      CU : constant GPR2.Build.Compilation_Unit.Object :=
        Unit_Name_To_Unit (Tree, Unit_Name);
   begin

      Traces.Trace
        (" Searching the compile action for unit " & String (Unit_Name));
      if CU.Is_Defined then
         for Action of Tree.Artifacts_Database.All_Actions loop
            if Action in GPR2.Build.Actions.Compile.Ada.Object'Class then
               declare
                  Act : constant GPR2.Build.Actions.Compile.Ada.Object :=
                    GPR2.Build.Actions.Compile.Ada.Object (Action);
               begin
                  if Act.Unit = CU then
                     Traces.Trace (" Found : " & Act.UID.Image);
                     return Act;
                  end if;
               end;
            end if;
         end loop;
      end if;

      Traces.Trace
        ("Did not find any Ada compile action related to the unit "
         & String (Unit_Name));
      return GPR2.Build.Actions.Compile.Ada.Undefined;
   end Unit_Name_To_Ada_Compile_Action;

   -----------------------
   -- Unit_Name_To_Unit --
   -----------------------

   function Unit_Name_To_Unit
     (Tree : GPR2.Project.Tree.Object; Unit_Name : Name_Type)
      return GPR2.Build.Compilation_Unit.Object
   is
      use GPR2.Build.Compilation_Unit;
      CU : GPR2.Build.Compilation_Unit.Object;
   begin

      for View of Tree.Ordered_Views loop
         CU := View.Own_Unit (Unit_Name);
         if CU.Is_Defined then
            return CU;
         end if;
      end loop;

      return GPR2.Build.Compilation_Unit.Undefined;
   end Unit_Name_To_Unit;

   Actions_Of_Interest : GPR2.Build.Tree_Db.Action_Sets.Set;
   Populate_Mains_Only : Boolean := False;
begin
   GNATCOLL.Traces.Parse_Config_File;
   GPRtools.Util.Set_Program_Name ("gprls");
   GPRls.Options.Setup (Parser);
   GPRls.Options.Parse_Command_Line (Parser, Opt);

   if not GPRtools.Options.Load_Project (Opt, GPR2.No_Error) then
      Handle_Program_Termination (Message => "");
   end if;

   if Opt.Tree.Has_Configuration
     and then Opt.Tree.Configuration.Log_Messages.Has_Element
                (Error    => True,
                 Warning  => True,
                 Hint     => False,
                 End_User => True)
   then
      Opt.Tree.Reporter.Report
        (GPR2.Message.Create
           (GPR2.Message.Warning,
            "Listing may be incomplete, as there were problems during"
            & " auto-configuration",
            Source_Reference.Create
              (Opt.Tree.Root_Project.Path_Name.Value, 0, 0)));
   end if;

   Opt.Tree.Update_Sources;
   if Opt.Closure_Mode and then Opt.Files.Is_Empty then
      Populate_Mains_Only := True;
   end if;

   if not GPR2.Build.Actions_Population.Populate_Actions
            (Opt.Tree,
             Opt.Build_Options,
             Static_Actions      => True,
             Populate_Mains_Only => Populate_Mains_Only)
   then
      return To_Exit_Status (E_Abort);
   end if;

   if not Opt.Files.Is_Empty then

      --  Ensure that all specified files can be found in the tree and
      --  determine the set of actions to consider.

      for File of Opt.Files loop
         if Opt.Closure_Mode then
            GPR2.Build.Tree_Db.Action_Sets.Union
              (Actions_Of_Interest,
               Compute_Closure (Opt.Tree, Filename_Type (File)));
         else
            declare
               Act : constant GPR2.Build.Actions.Compile.Ada.Object :=
                 Source_To_Ada_Compile_Action (Opt.Tree, Filename_Type (File));
            begin
               if not Act.Is_Defined then
                  Opt.Tree.Reporter.Report
                    (GPR2.Message.Create
                       (GPR2.Message.Warning,
                        "Can not find Ada compile action for source file "
                        & String (File)));
               else
                  Actions_Of_Interest.Include (Act.UID);
               end if;
            end;
         end if;
      end loop;
   end if;

   for Action of Opt.Tree.Artifacts_Database.All_Actions loop
      if (not Opt.Files.Is_Empty
          and then Actions_Of_Interest.Contains (Action.UID))
        or else (Opt.Files.Is_Empty
                 and then Action
                          in GPR2.Build.Actions.Compile.Ada.Object'Class)
      then
         declare
            Act : GPR2.Build.Actions.Compile.Ada.Object'Class :=
              GPR2.Build.Actions.Compile.Ada.Object'Class (Action);
            --  The action must be modifiable, so that its signature can be
            --  updated.

            Runtime_Deps     : GPR2.Path_Name.Set.Object;
            Non_Runtime_Deps : GPR2.Path_Name.Set.Object;
            Source_Path      : GPR2.Path_Name.Object;

         begin
            if Opt.All_Projects or else not Action.View.Is_Externally_Built
            then

               Act.Load_Signature (Check_Checksums => False);

               --  Fetch the dependencies of the compilation action if needed
               if Opt.With_Predefined_Units or else Opt.Dependency_Mode then
                  if Act in GPR2.Build.Actions.Compile.Object'Class then
                     for Dep of Act.Dependencies loop
                        for View of Opt.Tree.Ordered_Views loop
                           Source_Path :=
                             View.Source_Path
                               (Name            =>
                                  GPR2.Path_Name.Simple_Name (Dep),
                                Allow_Spec_File => True,
                                Allow_Unit_Name => False);
                           if Source_Path /= GPR2.Path_Name.Undefined then
                              if View.Is_Runtime then
                                 Runtime_Deps.Append (Source_Path);
                              else
                                 Non_Runtime_Deps.Append (Source_Path);
                              end if;
                           end if;
                        end loop;
                     end loop;
                  end if;
               end if;

               declare
                  Signature : constant GPR2.Build.Signature.Object :=
                    Act.Signature;
               begin
                  Print_Action
                    (Act, Runtime_Deps, Non_Runtime_Deps, Signature);
               end;
            end if;
         end;
      end if;
   end loop;

   return To_Exit_Status (E_Success);

exception
   when E : GPR2.Options.Usage_Error =>
      Handle_Program_Termination
        (Display_Command_Line_Help => True,
         Force_Exit                => False,
         Message                   => Exception_Message (E));
      return To_Exit_Status (E_Fatal);

   when Project_Error =>
      Handle_Program_Termination
        (Force_Exit => False,
         Message    =>
           '"'
           & String (Opt.Project_File.Simple_Name)
           & """ processing failed");
      return To_Exit_Status (E_Fatal);

   when E_Program_Termination =>
      return To_Exit_Status (E_Fatal);

   when E : others =>
      Handle_Program_Termination
        (Force_Exit => False,
         Exit_Cause => E_Generic,
         Message    => Exception_Information (E));
      return To_Exit_Status (E_Fatal);
end GPRls.Main;
