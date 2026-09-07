--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  An action that links, or has something else link for it

package GPR2.Build.Actions.Link_Options_Consumer is

   type Object is interface;

   procedure Add_Option_From_Binder
     (Self : in out Object; Option : String) is abstract;
   --  Add an option to the link this action leads to

end GPR2.Build.Actions.Link_Options_Consumer;
