--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Containers;
with GPR2.Project.Tree;
with GPR2.Source_Reference.Value;

package GPR2.Project.Source_Files is

   package Source_Set renames Containers.Filename_Type_Set;

   procedure Read
     (Tree     : not null access Project.Tree.Object;
      Filename : GPR2.Path_Name.Full_Name;
      Attr     : Source_Reference.Value.Object;
      Set      : in out Source_Set.Set);

end GPR2.Project.Source_Files;
