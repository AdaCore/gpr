--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Environment_Variables;

package body GPR2.Environment is

   ------------
   -- Exists --
   ------------

   function Exists (Environment : Object; Key : String) return Boolean is
   begin
      return Environment.Dict.Contains (Key)
        or else (Environment.Inherit_Process_Env
                 and then Ada.Environment_Variables.Exists (Key));

   end Exists;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Environment : in out Object; Key : String; Value : String) is
      use GNATCOLL.OS.Process.Env_Dicts;

      C : constant Cursor := Environment.Dict.Find (Key);
   begin
      if C = No_Element then
         Insert (Environment.Dict, Key, Value);
      else
         Replace_Element (Environment.Dict, C, Value);
      end if;
   end Insert;

   -----------------
   -- Set_Inherit --
   -----------------

   procedure Set_Inherit (Environment : in out Object; Inherit : Boolean) is
   begin
      Environment.Inherit_Process_Env  := Inherit;
   end Set_Inherit;

   -----------
   -- Value --
   -----------

   function Value (Environment : Object; Key : String) return String is
      use GNATCOLL.OS.Process.Env_Dicts;

      C : constant Cursor := Environment.Dict.Find (Key);
   begin
      if C /= No_Element then
         return  Element (C);
      elsif Environment.Inherit_Process_Env  then
         return Ada.Environment_Variables.Value (Key);
      else
         raise Constraint_Error;
      end if;
   end Value;

   function Value
     (Environment : Object; Key : String; Default : String) return String is
   begin
      if Exists (Environment, Key) then
         return Value (Environment, Key);
      else
         return Default;
      end if;
   end Value;

end GPR2.Environment;
