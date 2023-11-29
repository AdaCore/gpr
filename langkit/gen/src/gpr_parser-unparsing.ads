
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Gpr_Parser.Analysis; use Gpr_Parser.Analysis;

package Gpr_Parser.Unparsing is

   function Unparse (Node : Gpr_Node'Class) return String
      with Pre => not Node.Unit.Has_Diagnostics;
   --  Turn the Node tree into a string that can be re-parsed to yield the same
   --  tree (source locations excepted). The encoding used is the same as the
   --  one that was used to parse Node's analysis unit.
   --
   --  Note that this requires that Node's unit has no parsing error.

end Gpr_Parser.Unparsing;
