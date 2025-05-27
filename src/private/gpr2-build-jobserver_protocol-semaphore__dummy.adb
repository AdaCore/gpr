--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Jobserver_Protocol.Semaphore is

   --  Dummy implementation for Unix

   ---------------
   -- Get_Token --
   ---------------

   overriding function Get_Token
     (Self  : Object;
      Token : out Character) return Boolean
   is
      pragma Unreferenced (Token);
   begin
      return False;
   end Get_Token;

   ----------------
   -- Initialize --
   ----------------

   overriding function Initialize (Param : String) return Object
   is
   begin
      return (others => <>);
   end Initialize;

   ------------------
   -- Is_Available --
   ------------------

   overriding function Is_Available (Self : Object) return Boolean is
   begin
      return False;
   end Is_Available;

   -------------------
   -- Release_Token --
   -------------------

   overriding procedure Release_Token
     (Self  : Object;
      Token : Character) is null;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Object) is null;

end GPR2.Build.Jobserver_Protocol.Semaphore;
