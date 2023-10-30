--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.KB;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

procedure Main is
   KB : GPR2.KB.Object := GPR2.KB.Create (GPR2.KB.Default_Flags);
   S  : String := Ada.Strings.Unbounded.To_String
                          (GPR2.KB.Known_Compiler_Names(KB));
begin

   --  Known compilers list may change. To avoid frequent update of this test,
   --  only one compiler is checked.

   if Ada.Strings.Fixed.Index (S, "CLANG-C") > 0 then
      Ada.Text_IO.Put_Line ("OK");
   else
      Ada.Text_IO.Put_Line ("KO");
   end if;
end Main;
