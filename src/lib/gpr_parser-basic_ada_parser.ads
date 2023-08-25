--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Gpr_Parser_Support.Diagnostics;
with Gpr_Parser_Support.Text;
with Gpr_Parser.Analysis;

package Gpr_Parser.Basic_Ada_Parser is

   procedure Parse_Context_Clauses
     (Filename       : String;
      Context        : Gpr_Parser.Analysis.Analysis_Context'Class;
      Log_Error      : access procedure (Message : String);
      With_Clause_CB : access procedure (Unit_Name  : String;
                                         Is_Limited : Boolean) := null;
      Unit_Name_CB   : access procedure (Unit_Name     : String;
                                         Separate_From : String;
                                         Generic_Unit  : Boolean) := null;
      No_Body_CB     : access procedure := null);
   --  Basic parser to find out the with clauses, the unit name and kind, with
   --  a special case to detect pragma No_Body to skip the source.
   --  Note: multi-unit sources are not supported, as a full Ada parser is
   --  needed to reach the end of a library level unit.
   --  * Context: ???
   --  * With_Clause_CB: callback called when a with clause is found.
   --  * Unit_Name_CB: called when a library level Unit or a Subunit is parsed.
   --  * No_Body_CB: called when the source has "pragma No_Body"

end Gpr_Parser.Basic_Ada_Parser;
