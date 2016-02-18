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
with GPR2.Project.Attribute.Set;
with GPR2.Project.Variable.Set;

private with GPR_Parser.Analysis;
private with GNAT.MD5;

package GPR2.Parser.Project is

   type Object is tagged private;

   Undefined : constant Object;

   function Load (Filename : Path_Name_Type) return Object;
   --  Phase-1: syntax parsing of the given project name. If an error occurs
   --  during the parsing the return object is Undefined.

   procedure Parse
     (Project : in out Object;
      Ctx     : Context.Object;
      Attrs   : in out GPR2.Project.Attribute.Set.Object;
      Vars    : in out GPR2.Project.Variable.Set.Object;
      Changed : not null access procedure (Project : Object))
     with Pre => Project /= Undefined;
   --  Phase-2: semantic analysis, parse tree using a specific context. This
   --  step is to be done every time a context is changed. The Changed callback
   --  is called whenever a specific project has been impacted by the context
   --  change.

   function Qualifier (Project : Object) return Project_Qualifier
     with Pre => Project /= Undefined;

   function Name (Project : Object) return Name_Type
     with Pre => Project /= Undefined;

   function Path_Name (Project : Object) return Path_Name_Type
     with Pre => Project /= Undefined;

   function Has_Imports (Project : Object) return Boolean
     with Pre => Project /= Undefined;
   --  Returns True if Project has some imported projects

   function Imports (Project : Object) return Containers.Path_Name_List
     with Pre => Project /= Undefined;

   function Has_Externals (Project : Object) return Boolean
     with Pre => Project /= Undefined;

   function Externals (Project : Object) return Containers.Name_List
     with Pre => Project /= Undefined;

private

   use Ada.Strings.Unbounded;
   use GNAT;
   use GPR_Parser.Analysis;

   Default_Signature : constant MD5.Binary_Message_Digest := (others => 0);
   --  The default signature, this is the one used for project having no
   --  external variable.

   type Object is tagged record
      Name      : Unbounded_String;
      File      : Path_Name_Type;
      Qualifier : Project_Qualifier := Q_Standard;
      Externals : Containers.Name_List;
      Imports   : Containers.Path_Name_List;
      Unit      : Analysis_Unit;
      Context   : Analysis_Context;
      Signature : MD5.Binary_Message_Digest;
      --  ??? Must be freed : Destroy (Context)
   end record;

   Undefined : constant Object := (others => <>);

end GPR2.Parser.Project;
