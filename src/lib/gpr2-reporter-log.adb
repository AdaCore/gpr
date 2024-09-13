--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Reporter.Log is

   ----------------
   -- Clear_Logs --
   ----------------

   procedure Clear_Log (Self : in out Object) is
   begin
      Self.Log.Clear;
   end Clear_Log;

   ------------
   -- Create --
   ------------

   function Create (Verbosity : Verbosity_Level := Regular) return Object
   is
   begin
      return (Verbosity => Verbosity, others => <>);
   end Create;

   ---------------------
   -- Internal_Report --
   ---------------------

   overriding procedure Internal_Report
     (Self : in out Object; Message : GPR2.Message.Object)
   is
   begin
      Self.Log.Append (Message);
   end Internal_Report;

   -------------------
   -- Set_Verbosity --
   -------------------

   procedure Set_Verbosity (Self : in out Object; Verbosity : Verbosity_Level)
   is
   begin
      Self.Verbosity := Verbosity;
   end Set_Verbosity;

   ---------------
   -- Verbosity --
   ---------------

   overriding function Verbosity (Self : Object) return Verbosity_Level is
   begin
      return Self.Verbosity;
   end Verbosity;

end GPR2.Reporter.Log;
