--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package GPR2.Build.Actions.Thread is

   type Object is abstract new GPR2.Build.Actions.Object with private;
   --  Base type for actions executed as Ada threads

   function Execute
    (Self   : in out Object;
     Stdout : in out Unbounded_String;
     Stderr : in out Unbounded_String) return Integer is abstract;
   --  Execute the action in the same thread as the caller.
   --  Returns 0 on success.

private

   type Object is abstract new GPR2.Build.Actions.Object with null record;

end GPR2.Build.Actions.Thread;
