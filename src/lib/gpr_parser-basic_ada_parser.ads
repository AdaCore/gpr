--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Gpr_Parser_Support.Diagnostics;
with Gpr_Parser_Support.Text;
with Gpr_Parser.Analysis;

package Gpr_Parser.Basic_Ada_Parser is

   type Library_Item_Type is (Is_Package, Is_Subprogram);
   package GPS renames Gpr_Parser_Support;

   procedure Parse_Context_Clauses
     (Filename       : String;
      Diagnostics    : in out GPS.Diagnostics.Diagnostics_Vectors
                          .Vector;
      Context        : Gpr_Parser.Analysis.Analysis_Context'Class;
      Charset        : String := "ISO-8859-1";
      With_Clause_CB : access procedure
        (Unit_Name  : String;
         Source_Loc : GPS.Slocs.Source_Location;
         Is_Limited : Boolean) :=
        null;
      Unit_Name_CB   : access procedure
        (Unit_Name     : String; Separate_From : String := "";
         Source_Loc    : GPS.Slocs.Source_Location;
         Lib_Item_Type : Gpr_Parser.Basic_Ada_Parser.Library_Item_Type;
         Generic_Unit  : Boolean) :=
        null;
      On_No_Body_CB  : access procedure := null);
   --  Parse context clauses, which can be summarized as zero,
   --  one or several with clauses, and one library item or subunit clause.
   --  Multi-units, which means having several library item clauses,
   --  is not supported.
   --  Set charset to "" if the byte order mark (BOM) needs to be read instead.

end Gpr_Parser.Basic_Ada_Parser;
