with GPR2.Project.Tree;

   --  Helpers to add the actions building blocks required by the tools.
package GPRtools.Actions is

   function Add_Actions_To_Build_Mains
     (Tree : GPR2.Project.Tree.Object) return Boolean;

   --  Add the pre-bind, post-bind, bind, and link actions required to generate
   --  an executable for each main. Only the actions directly linked to each
   --  source are added. Other actions, such as dependency compilation,
   --  will be added dynamically by the process manager.
   --
   --  Note that tree sources must be up-to-date before calling this function,
   --  as mains are identified by examining the tree.
   --
   --  Tree: project tree that will contain the new actions.
   --
   --  Returns True on success..

end GPRtools.Actions;
