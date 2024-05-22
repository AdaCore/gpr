--
--  Copyright (C) 2022-2024, AdaCore
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

end GPR2.Build.Source_Base.Ada_Parser;
