--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Artifacts.File_Part is

   overriding function Create (S : String) return Object is
      Idx : Natural := 0;
   begin
      for J in reverse S'Range loop
         if S (J) not in '0' .. '9' then
            exit;
         elsif S (J) = '@' then
            Idx := J;
            exit;
         end if;
      end loop;

      if Idx = 0 then
         return (Files.Create (S) with Index => No_Index);
      else
         return (Files.Create (S (S'First .. Idx - 1))
                 with Index => Unit_Index'Value (S (Idx + 1 .. S'Last)));
      end if;
   end Create;

end GPR2.Build.Artifacts.File_Part;
