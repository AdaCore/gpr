--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.File_Readers;

package GPR2.Build.Source_Base.Ada_Parser is

   procedure Compute
     (File_Reader      : GPR2.File_Readers.File_Reader_Reference;
      Data             : in out Source_Base.Object'Class;
      Get_Withed_Units : Boolean;
      Success          : out Boolean);
   --  Setup Data with the information from parsing Ada source file

   procedure Parse_Context_Clauses
     (Buffer         : String;
      Log_Error      : access procedure (Message : String);
      With_Clause_CB : access procedure (Unit_Name  : String;
                                         Is_Limited : Boolean) := null;
      Unit_Name_CB   : access procedure (Unit_Name     : String;
                                         Separate_From : String;
                                         Generic_Unit  : Boolean) := null;
      No_Body_CB     : access procedure := null);
   --  Report with clauses, the unit name and pragma No_Body through callbacks.
   --  Parse only the first unit header; multi-unit sources are unsupported.
   --  Buffer is Latin-1 unless a BOM specifies UTF-8, UTF-16 or UTF-32.
   --  Unit names passed to callbacks are UTF-8.

end GPR2.Build.Source_Base.Ada_Parser;
