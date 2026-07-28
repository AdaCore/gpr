--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  Prints several lines on standard output producing bare LF on
--  Unix and CR/LF on Windows, as standard output is in text mode
--  in contrary to the binary one.

with Ada.Text_IO;

procedure Printer is
begin
   Ada.Text_IO.Put_Line ("line 1");
   Ada.Text_IO.Put_Line ("line 2");
   Ada.Text_IO.Put_Line ("line 3");
end Printer;
