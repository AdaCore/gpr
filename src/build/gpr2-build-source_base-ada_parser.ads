--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.File_Readers;

private with Gpr_Parser.Basic_Ada_Parser;

package GPR2.Build.Source_Base.Ada_Parser is

   type Parser_State is private;
   function Create_New_State return Parser_State;
   procedure Close (State : in out Parser_State);

   procedure Compute
     (File_Reader      : GPR2.File_Readers.File_Reader_Reference;
      State            : Parser_State;
      Data             : in out Source_Base.Object'Class;
      Get_Withed_Units : Boolean;
      Success          : out Boolean);
   --  Setup Data with the information from parsing Ada source file

private

   type Parser_State is new Gpr_Parser.Basic_Ada_Parser.Iconv_States;

end GPR2.Build.Source_Base.Ada_Parser;
