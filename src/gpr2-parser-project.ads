------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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

with GPR2.Containers;
with GPR2.Context;
with GPR2.Log;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Pack.Set;
with GPR2.Project.Variable.Set;

limited with GPR2.Project.Tree;

private with GPR_Parser.Analysis;

package GPR2.Parser.Project is

   type Object is tagged private;

   subtype Project_Type is Object;

   Undefined : constant Object;

   function Load
     (Filename : Path_Name_Type;
      Messages : out Log.Object) return Object;
   --  Phase-1: syntax parsing of the given project name. If an error occurs
   --  during the parsing the return object is Undefined.

   procedure Parse
     (Self    : in out Object;
      Tree    : GPR2.Project.Tree.Object;
      Context : GPR2.Context.Object;
      Attrs   : in out GPR2.Project.Attribute.Set.Object;
      Vars    : in out GPR2.Project.Variable.Set.Object;
      Packs   : in out GPR2.Project.Pack.Set.Object)
     with Pre => Self /= Undefined;
   --  Phase-2: semantic analysis, parse tree using a specific context. This
   --  step is to be done every time a context is changed. The Changed callback
   --  is called whenever a specific project has been impacted by the context
   --  change.

   function Qualifier (Self : Object) return Project_Kind
     with Pre => Self /= Undefined;
   --  Returns the project qualifier if present when parsing. Returns
   --  Q_Standard if no qualifier is present. Note that the actual project
   --  kind may be different as computed based on the attributes present on
   --  the project.

   function Has_Extended (Self : Object) return Boolean;
   --  Returns True if an extended project is defined

   function Extended (Self : Object) return Path_Name_Type
     with Pre => Self.Has_Extended;
   --  Returns the extended project

   function Name (Self : Object) return Name_Type
     with Pre => Self /= Undefined;
   --  The name of the project file

   function Path_Name (Self : Object) return Path_Name_Type
     with Pre => Self /= Undefined;
   --  The full path name of the project file

   function Has_Imports (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if Project has some imported projects

   function Imports (Self : Object) return Containers.Path_Name_List
     with Pre  => Self /= Undefined,
          Post => (if Self.Has_Imports
                   then not Imports'Result.Is_Empty
                   else Imports'Result.Is_Empty);
   --  Returns the list of path name for all imported projects

   function Has_Externals (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if the project has some external variable reference

   function Externals (Self : Object) return Containers.Name_List
     with Pre  => Self /= Undefined,
          Post => (if Self.Has_Externals
                   then not Externals'Result.Is_Empty
                   else Externals'Result.Is_Empty);
   --  Returns the list of all external variables

private

   use GPR_Parser.Analysis;

   type Object is tagged record
      Name      : Unbounded_String;
      File      : Path_Name_Type;
      Qualifier : Project_Kind := K_Standard;
      Externals : Containers.Name_List;
      Imports   : Containers.Path_Name_List;
      Extended  : Path_Name_Type;
      Unit      : Analysis_Unit;
      Context   : Analysis_Context;
      --  ??? Must be freed : Destroy (Context)
   end record;

   Undefined : constant Object := (others => <>);

end GPR2.Parser.Project;
