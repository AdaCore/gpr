------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Import.Set;
with GPR2.Project.Pack.Set;
with GPR2.Project.Typ.Set;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

limited with GPR2.Project.Tree;

with GPR_Parser.Analysis;

package GPR2.Parser.Project is

   use GPR_Parser.Analysis;

   type Object is tagged private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Parse
     (Filename      : GPR2.Path_Name.Object;
      Implicit_With : GPR2.Path_Name.Set.Object;
      Messages      : out Log.Object) return Object;
   --  Phase-1: syntax parsing of the given project name. If an error occurs
   --  during the parsing the return object is Undefined.

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

   procedure Process
     (Self    : in out Object;
      Tree    : GPR2.Project.Tree.Object;
      Context : GPR2.Context.Object;
      View    : GPR2.Project.View.Object;
      Attrs   : in out GPR2.Project.Attribute.Set.Object;
      Vars    : in out GPR2.Project.Variable.Set.Object;
      Packs   : in out GPR2.Project.Pack.Set.Object;
      Types   : in out GPR2.Project.Typ.Set.Object)
     with Pre => Self.Is_Defined;
   --  Phase-2: semantic analysis, parse tree using a specific context. This
   --  step is to be done every time a context is changed.

   function Qualifier (Self : Object) return Project_Kind
     with Pre => Self.Is_Defined;
   --  Returns the project qualifier if present when parsing. Returns
   --  Q_Standard if no qualifier is present. Note that the actual project
   --  kind may be different as computed based on the attributes present on
   --  the project.

   function Explicit_Qualifier (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if project qualifier defined explicitly

   function Has_Extended (Self : Object) return Boolean;
   --  Returns True if an extended project is defined

   function Is_Extending_All (Self : Object) return Boolean
     with Pre => Self.Has_Extended;
   --  Returns True if the extended project is an extends all

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
   --  Returns the GPR_Parser analysis unit

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

end GPR2.Parser.Project;
