--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Path_Name;

private package GPR2.Project_Parser.Registry is

   use type GPR2.Path_Name.Object;

   function Exists (Pathname : GPR2.Path_Name.Object) return Boolean
     with Pre  => Pathname.Is_Defined;
   --  Returns True if the project file given by its full path-name is known in
   --  the registry.

   procedure Register
     (Pathname : GPR2.Path_Name.Object; Project : Object);
   --  Registers a new project syntactic tree for Pathname

   function Check_Project
     (Pathname : GPR2.Path_Name.Object;
      Project  : out Object) return Boolean
     with Pre  => Pathname.Is_Defined,
          Post => Check_Project'Result = Project.Is_Defined;
   --  Returns True if the project file given by its full path-name is known in
   --  the registry and puts it into the Project out parameter.
   --  Returns False otherwise and puts Undefined into Project out parameter.

   function Get
     (Pathname : GPR2.Path_Name.Object) return Object;
   --  Returns the syntactic tree for project Pathname

   procedure Clear_Cache;
   --  Clears the parsed objects cache

end GPR2.Project_Parser.Registry;
