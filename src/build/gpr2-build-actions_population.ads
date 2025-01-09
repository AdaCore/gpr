--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Ordered_Maps;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Options;
with GPR2.Containers;
with GPR2.Project.Tree;

package GPR2.Build.Actions_Population is

   package Lang_Args is new Ada.Containers.Ordered_Maps
     (GPR2.Language_Id, GPR2.Containers.Value_List,
      GPR2."<",
      GPR2.Containers.Value_Type_List."=");

   function Resolve_Mains
     (Tree    : GPR2.Project.Tree.Object;
      Options : GPR2.Build.Options.Build_Options;
      Error   : out Boolean)
      return GPR2.Build.Compilation_Unit.Unit_Location_Vector;
   --  Utility function to resolve the actual list of Mains to consider for
   --  the given Build_Options. Error is set upon error retrieving an actual
   --  main.

   function Populate_Actions
     (Tree    : GPR2.Project.Tree.Object;
      Options : GPR2.Build.Options.Build_Options) return Boolean;

end GPR2.Build.Actions_Population;
