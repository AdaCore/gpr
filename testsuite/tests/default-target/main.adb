--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.KB;
with Ada.Text_IO;
procedure Main is
begin
   Ada.Text_IO.Put_Line (String (GPR2.KB.Default_Target));
end Main;
