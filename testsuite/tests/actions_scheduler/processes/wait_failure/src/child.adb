--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;
procedure Child is
begin
   Ada.Text_IO.Put_Line ("child started");
   loop
      delay 1.0;
   end loop;
end Child;
