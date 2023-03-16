
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Gpr_Parser.Implementation;
use Gpr_Parser.Implementation;

with Gpr_Parser.Public_Converters; use Gpr_Parser.Public_Converters;
with Gpr_Parser.Unparsing_Implementation;
use Gpr_Parser.Unparsing_Implementation;

package body Gpr_Parser.Unparsing is

   -------------
   -- Unparse --
   -------------

   function Unparse (Node : Gpr_Node'Class) return String is
      N : constant Bare_Gpr_Node := Unwrap_Node (Node);
   begin
      return Unparse
        (Create_Abstract_Node (N),
         Unwrap_Node (Node).Unit,
         Preserve_Formatting => False,
         As_Unit             => False);
   end Unparse;

end Gpr_Parser.Unparsing;
