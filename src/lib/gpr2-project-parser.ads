--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Import.Set;
with GPR2.Project.Typ.Set;
with GPR2.Project.View;

limited with GPR2.Project.Tree;

with Gpr_Parser.Analysis;
with Gpr_Parser_Support.File_Readers;

package GPR2.Project.Parser is

   use Gpr_Parser.Analysis;

   type Object is tagged private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Parse
     (Filename      : GPR2.Path_Name.Object;
      Implicit_With : GPR2.Path_Name.Set.Object;
      Messages      : in out Log.Object;
      File_Reader   : Gpr_Parser_Support.File_Readers.File_Reader_Reference :=
                        Gpr_Parser_Support.File_Readers.
                          No_File_Reader_Reference)
      return Object;
   --  Phase-1: syntax parsing of the given project name. If an error occurs
   --  during the parsing the return object is Undefined.
   --  File_Reader provides an interface to abstract away the action of reading
   --  a source file to parse. Depending on use cases, it allows to override
   --  bytes-to-text decoding and preprocess sources (before actual
   --  lexing/parsing) or reading a source file from memory instead of disk.

   function Parse
     (Contents        : Ada.Strings.Unbounded.Unbounded_String;
      Messages        : out Log.Object;
      Pseudo_Filename : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined)
      return Object;
   --  Performs phase-1 on Contents. Project parsed this way is not registered
   --  in the Registry. Pseudo_Filename is used for reporting possible errors,
   --  when not specified the default /string_input/default.gpr is used.
   --  This is mostly intended to load configuration projects created in memory
   --  to be used during autoconfiguration step.

   procedure Clear_Cache
     (Filename : GPR2.Path_Name.Object);
   --  Clears the parsed objects cache for Filename

   procedure Process
     (Self          : in out Object;
      Tree          : GPR2.Project.Tree.Object;
      Context       : GPR2.Context.Object;
      View          : GPR2.Project.View.Object;
      Pre_Conf_Mode : Boolean := False;
      Ext_Conf_Mode : Boolean := False)
     with Pre => Self.Is_Defined;
   --  Phase-2: semantic analysis, parse tree using a specific context. This
   --  step is to be done every time a context is changed.
   --  Pre_Conf_Mode indicates that processing errors related to missing
   --  projects should be treated as warnings.
   --  Ext_Conf_Mode indicates that undefined externals should be ignored,
   --  this mode should only be used when processing configuration project.

   function Qualifier (Self : Object) return Project_Kind
     with Pre => Self.Is_Defined;
   --  Returns the project qualifier if present when parsing. Returns
   --  Q_Standard if no qualifier is present. Note that the actual project
   --  kind may be different as computed based on the attributes present on
   --  the project.

   function Explicit_Qualifier (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if project qualifier defined explicitly

   function Has_Extended (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if an extended project is defined

   function Is_Extending_All (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the project is an extends all

   function Extended (Self : Object) return GPR2.Project.Import.Object
     with Pre => Self.Has_Extended;
   --  Returns the extended project

   function Name (Self : Object) return Name_Type
     with Pre => Self.Is_Defined;
   --  The name of the project file

   function Path_Name (Self : Object) return Path_Name.Object
     with Pre => Self.Is_Defined;
   --  The full path name of the project file

   function Has_Imports (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Project has some imported projects

   function Imports (Self : Object) return GPR2.Project.Import.Set.Object
     with Pre  => Self.Is_Defined,
          Post => (if Self.Has_Imports
                   then not Imports'Result.Is_Empty
                   else Imports'Result.Is_Empty);
   --  Returns the list of path name for all imported projects

   function Has_Externals (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the project has some external variable reference

   function Externals (Self : Object) return Containers.Name_List
     with Pre  => Self.Is_Defined,
          Post => (if Self.Has_Externals
                   then not Externals'Result.Is_Empty
                   else Externals'Result.Is_Empty);
   --  Returns the list of all external variables

   function Unit (Self : Object) return Analysis_Unit
     with Pre => Self.Is_Defined;
   --  Returns the Gpr_Parser analysis unit

   function Skip_Sources
     (Self : Object) return Containers.Filename_Source_Reference;
   --  Source filenames to skip due to inactive case items

private

   type Object is tagged record
      Name      : Unbounded_String;
      File      : GPR2.Path_Name.Object;
      Qualifier : Project_Kind := K_Standard;
      Expl_Qual : Boolean      := False; -- Explicit qualifier
      Externals : Containers.Name_List;
      Imports   : GPR2.Project.Import.Set.Object;
      Extended  : GPR2.Project.Import.Object;
      Is_All    : Boolean := False;
      Unit      : Analysis_Unit := No_Analysis_Unit;
      Types     : GPR2.Project.Typ.Set.Object;
      Context   : Analysis_Context := No_Analysis_Context;
      Skip_Src  : Containers.Filename_Source_Reference;
      --  Naming exception source files to be ignored due to inactive case
      --  alternatives.
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Skip_Sources
     (Self : Object) return Containers.Filename_Source_Reference
   is
     (Self.Skip_Src);

   function Explicit_Qualifier (Self : Object) return Boolean is
     (Self.Expl_Qual);

end GPR2.Project.Parser;
