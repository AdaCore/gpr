
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Gpr_Parser_Support;
with Gpr_Parser_Support.Diagnostics;
with Gpr_Parser_Support.Slocs;
with Gpr_Parser_Support.Text;

package Gpr_Parser is

   Version    : constant String := "undefined";
   Build_Date : constant String := "undefined";

   --  Gpr_Parser's main entry point is the Gpr_Parser.Analysis
   --  package.

   --  Convenience renaming for support package that Langkit provides

   package Support renames Gpr_Parser_Support;
   package Diagnostics renames Support.Diagnostics;
   package Slocs renames Support.Slocs;
   package Text renames Support.Text;

end;
