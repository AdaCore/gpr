--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Interfaces;

with GNATCOLL.Traces;

package body GPR2.Build.Jobserver_Protocol.Semaphore is

   --  Implementation for Windows

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPR.BUILD.JOBSERVER.PROTOCOL.SEMAPHORE", GNATCOLL.Traces.Off);

   function Open_Semaphore
     (Desired_Access : Interfaces.Unsigned_32;
      Inherit_Handle : Boolean;
      Name           : System.Address) return Integer
   with Import, Convention => Stdcall, External_Name => "OpenSemaphoreA";

   function Release_Semaphore
     (Semaphore      : Integer;
      Release_Count  : Long_Integer;
      Previous_Count : access Long_Integer) return Boolean
   with Import, Convention => Stdcall, External_Name => "ReleaseSemaphore";

   function Wait_For_Object
     (Semaphore : Integer; Milliseconds : Interfaces.Unsigned_32)
      return Interfaces.Unsigned_32
   with Import, Convention => Stdcall, External_Name => "WaitForSingleObject";

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out Object) is
   begin
      Self.Handle := 0;
   end Finalize;

   ---------------
   -- Get_Token --
   ---------------

   overriding
   function Get_Token (Self : Object; Token : out Character) return Boolean is
      use type Interfaces.Unsigned_32;
   begin
      if Wait_For_Object (Self.Handle, 0) /= 0 then
         return False;
      end if;

      Token := '+';
      return True;
   end Get_Token;

   ----------------
   -- Initialize --
   ----------------

   overriding
   function Initialize (Param : String) return Object is
      Result : Object;
   begin
      Traces.Trace ("opening a semaphore protocol for " & Param);
      Result.Handle := Open_Semaphore (16#1F0003#, False, Param'Address);
      return Result;
   end Initialize;

   ------------------
   -- Is_Available --
   ------------------

   overriding
   function Is_Available (Self : Object) return Boolean is
   begin
      return (Self.Handle /= 0);
   end Is_Available;

   -------------------
   -- Release_Token --
   -------------------

   overriding
   procedure Release_Token (Self : Object; Token : Character) is
      Ign : Boolean;
      pragma Unreferenced (Token, Ign);
   begin
      Ign := Release_Semaphore (Self.Handle, 1, null);
   end Release_Token;

end GPR2.Build.Jobserver_Protocol.Semaphore;
