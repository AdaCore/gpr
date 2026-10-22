--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

private package GPR2.Unicode is
   pragma Elaborate_Body;

   function Fold (Text : String; Uppercase : Boolean := False) return String;
   --  Apply locale-independent Unicode simple case folding to UTF-8 text.
   --  Uppercase prefers uppercase characters within the same fold class.

end GPR2.Unicode;
