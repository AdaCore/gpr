
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;

with GNATCOLL.VFS;

with Gpr_Parser_Support.Diagnostics; use Gpr_Parser_Support.Diagnostics;
with Gpr_Parser_Support.Symbols;     use Gpr_Parser_Support.Symbols;

with Gpr_Parser_Support.Token_Data_Handlers;
use Gpr_Parser_Support.Token_Data_Handlers;

with Gpr_Parser.Common; use Gpr_Parser.Common;
limited with Gpr_Parser.Implementation;

private package Gpr_Parser.Lexer_Implementation is

   type Internal_Lexer_Input (Kind : Lexer_Input_Kind) is record
      case Kind is
      when File | Bytes_Buffer =>
         Charset  : Unbounded_String;
         Read_BOM : Boolean;

         case Kind is
            when File =>
               Filename : GNATCOLL.VFS.Virtual_File;
            when Bytes_Buffer =>
               Bytes       : System.Address;
               Bytes_Count : Natural;
            when others =>
               null;
         end case;

      when Text_Buffer =>
         Text       : System.Address;
         Text_Count : Natural;
      end case;
   end record;
   --  See Gpr_Parser.Lexer.Lexer_Input for details. Resources pointed by
   --  access types must be free'd by Extract_Tokens's caller when done with
   --  it.

   procedure Extract_Tokens
     (Input       : Internal_Lexer_Input;
      With_Trivia : Boolean;
      File_Reader : access Implementation.Internal_File_Reader'Class;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Implementation for Gpr_Parser.Lexer.Extract_Tokens

   function Get_Symbol
     (Token : Token_Or_Trivia_Index;
      TDH   : Token_Data_Handler) return Symbol_Type;
   --  Assuming that ``Token`` refers to a token that contains a symbol, return
   --  the corresponding symbol.

end Gpr_Parser.Lexer_Implementation;
