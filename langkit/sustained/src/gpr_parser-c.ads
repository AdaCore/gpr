
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with System;

with Gpr_Parser.Analysis; use Gpr_Parser.Analysis;

--  This package provides conversion helpers to switch between types as found
--  in Gpr_Parser's public Ada API and the corresponding C API. Use it
--  when interfacing with foreign code.

package Gpr_Parser.C is

   function C_Context (Context : Analysis_Context) return System.Address;
   function Ada_Context (Context : System.Address) return Analysis_Context;

   function C_Unit (Unit : Analysis_Unit) return System.Address;
   function Ada_Unit (Unit : System.Address) return Analysis_Unit;

end Gpr_Parser.C;
