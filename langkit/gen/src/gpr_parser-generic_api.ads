
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Gpr_Parser_Support.Generic_API; use Gpr_Parser_Support.Generic_API;
with Gpr_Parser_Support.Generic_API.Analysis;
use Gpr_Parser_Support.Generic_API.Analysis;

with Gpr_Parser.Analysis; use Gpr_Parser.Analysis;
with Gpr_Parser.Common;   use Gpr_Parser.Common;

package Gpr_Parser.Generic_API is

   Gpr_Lang_Id : constant Language_Id
     with Import, External_Name => "Gpr_Parser__language_id";
   --  Unique identifier for Gpr_Parser

   Self_Id : Language_Id renames Gpr_Lang_Id;
   --  Shortcut for convenience in code generation

   function To_Generic_Context (Context : Analysis_Context) return Lk_Context;
   --  Convert the given ``Context`` into a value suitable to use in the
   --  Langkit generic API.

   function From_Generic_Context
     (Context : Lk_Context) return Analysis_Context;
   --  Convert the ``Context`` value from the Langkit generic API into the
   --  Gpr_Parser-specific context type. Raise a
   --  ``Gpr_Parser_Support.Errors.Precondition_Failure`` if ``Context`` does not
   --  belong to Gpr_Parser.

   function To_Generic_Unit (Unit : Analysis_Unit) return Lk_Unit;
   --  Convert the given ``Unit`` into a value suitable to use in the Langkit
   --  generic API.

   function From_Generic_Unit (Unit : Lk_Unit) return Analysis_Unit;
   --  Convert the ``Unit`` value from the Langkit generic API into the
   --  Gpr_Parser-specific unit type. Raise a
   --  ``Gpr_Parser_Support.Errors.Precondition_Failure`` if ``Unit`` does not
   --  belong to Gpr_Parser.

   function To_Generic_Grammar_Rule
     (Rule : Grammar_Rule) return Gpr_Parser_Support.Generic_API.Grammar_Rule_Ref;
   --  Convert the given ``rule`` into a value suitable to use in the Langkit
   --  generic API.

   function From_Generic_Grammar_Rule
     (Rule : Gpr_Parser_Support.Generic_API.Grammar_Rule_Ref) return Grammar_Rule;
   --  Convert the ``Rule`` value from the Langkit generic API into the
   --  Gpr_Parser-specific unit type. Raise a
   --  ``Gpr_Parser_Support.Errors.Precondition_Failure`` if ``Rule`` does not
   --  belong to Gpr_Parser or if it is ``No_Grammar_Rule_Ref``.

   function To_Generic_Node
     (Node : Gpr_Node'Class) return Lk_Node;
   --  Convert the given ``Node`` into a value suitable to use in the Langkit
   --  generic API.

   function From_Generic_Node (Node : Lk_Node) return Gpr_Node;
   --  Convert the ``Node`` value from the Langkit generic API into the
   --  Gpr_Parser-specific unit type. Raise a
   --  ``Gpr_Parser_Support.Errors.Precondition_Failure`` if ``Node`` does not
   --  belong to Gpr_Parser.

end Gpr_Parser.Generic_API;
