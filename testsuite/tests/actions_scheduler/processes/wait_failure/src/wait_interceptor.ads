--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.Process;
package Wait_Interceptor is
   function Wait (Handle : GNATCOLL.OS.Process.Process_Handle) return Integer
     with Export, Convention => C,
          External_Name => "__wrap_gnatcoll__os__process__wait";
end Wait_Interceptor;
