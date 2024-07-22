with GPR2.Log;
with GPR2.Project.Tree;

   --  Helpers to add the actions building blocks required by the tools.
package GPRtools.Actions is

   function Add_Actions_To_Build_Mains
     (Tree : GPR2.Project.Tree.Object;
      Log  : out GPR2.Log.Object) return Boolean;

   --  Add the pre-bind, post-bind, bind, and link actions required to generate
   --  an executable for each main. Only the actions directly linked to each
   --  source are added. Other actions, such as dependency compilation,
   --  will be added dynamically by the process manager.
   --
   --  Note that tree sources must be up-to-date before calling this function,
   --  as mains are identified by examining the tree.
   --
   --  Tree: project tree that will contain the new actions.
   --  Log: Contains all the action manipulation messages.
   --
   --  If an error occurs, False is returned and an error message will be
   --  available in `Log`.

end GPRtools.Actions;
