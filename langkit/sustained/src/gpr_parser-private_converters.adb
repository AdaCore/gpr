
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Gpr_Parser_Support.Internal.Analysis; use Gpr_Parser_Support.Internal.Analysis;
with Gpr_Parser_Support.Internal.Conversions;
use Gpr_Parser_Support.Internal.Conversions;

with Gpr_Parser.Generic_API; use Gpr_Parser.Generic_API;

package body Gpr_Parser.Private_Converters is

   function "+"
     (Entity : Gpr_Parser_Support.Internal.Analysis.Internal_Entity)
      return Implementation.Internal_Entity
     with Import,
          External_Name => "Gpr_Parser__from_generic_internal_entity";
   function "+"
     (Entity : Implementation.Internal_Entity)
      return Gpr_Parser_Support.Internal.Analysis.Internal_Entity
     with Import,
          External_Name => "Gpr_Parser__to_generic_internal_entity";
   --  See the corresponding exports in $.Generic_Impl

   -----------------------
   -- From_Generic_Node --
   -----------------------

   function From_Generic_Node
     (Node : Lk_Node) return Implementation.Internal_Entity is
   begin
      return +Unwrap_Node (Node);
   end From_Generic_Node;

   ---------------------
   -- To_Generic_Node --
   ---------------------

   function To_Generic_Node
     (Entity : Implementation.Internal_Entity) return Lk_Node is
   begin
      return Wrap_Node (Self_Id, +Entity);
   end To_Generic_Node;

end Gpr_Parser.Private_Converters;
