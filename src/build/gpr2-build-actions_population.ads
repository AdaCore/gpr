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

   function Resolve_Mains_From_Options
     (Tree    : GPR2.Project.Tree.Object;
      Options : GPR2.Build.Options.Build_Options;
      Error   : out Boolean)
      return GPR2.Build.Compilation_Unit.Unit_Location_Vector;
   --  Utility function to resolve the actual list of Mains to consider for
   --  the given Build_Options. Error is set upon error retrieving an actual
   --  main.

   function Populate_Actions
     (Tree                  : GPR2.Project.Tree.Object;
      Options               : GPR2.Build.Options.Build_Options;
      Static_Actions        : Boolean;
      With_Externally_Built : Boolean := False;
      Populate_Mains_Only   : Boolean := False) return Boolean;
   --  Populate all the actions of the tree.
   --  @param Tree
   --    the current project tree
   --  @Param Options
   --    the options used to amend the population of actions
   --  @Param Static_Actions
   --    if unset, then the actions are to be executed by the process manager,
   --    else the population is performed so that all validated actions have
   --    their proper pre and post commands executed (and thus have all the
   --    output analysis performed) when their signature is valid.
   --  @Param Populate_Mains_Only
   --    if set, then only the actions required to build the mains specified in
   --    Options are populated.

end GPR2.Build.Actions_Population;
