--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Interfaces;

package body GPR2.Build.Jobserver.Semaphore is

   function Create (Name : String) return Object is
      function Open_Semaphore
        (Desired_Access : Interfaces.Unsigned_32;
         Inherit_Handle : Boolean;
         Name           : System.Address) return Integer
        with Import, Convention => Stdcall, External_Name => "OpenSemaphoreA";

      JS : Object;
   begin
      JS.Kind             := JM_Semaphore;
      JS.Traces.Trace ("Connection method : " & JS.Kind'Img);
      JS.Semaphore_Handle := Open_Semaphore (16#1F0003#, False, Name'Address);
      return JS;
   end Create;

   --------------
   -- Register --
   --------------

   overriding
   function Register (Self : Object; Char : out Character) return Boolean is
      use type Interfaces.Unsigned_32;

      function Wait_For_Object
        (Semaphore    : Integer;
         Milliseconds : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
        with Import, Convention => Stdcall,
             External_Name => "WaitForSingleObject";
   begin
      if Wait_For_Object (Self.Semaphore_Handle, 0) /= 0 then
         return False;
      end if;

      Char := '+';
      return True;
   end Register;

   -------------
   -- Release --
   -------------

   overriding
   function Release (Self : Object; Char : Character) return Boolean is
      function  Release_Semaphore
        (Semaphore      : Integer;
         Release_Count  : Long_Integer;
         Previous_Count : access Long_Integer) return Boolean
        with Import, Convention => Stdcall,
             External_Name => "ReleaseSemaphore";

      pragma Unreferenced (Char);
   begin
      if not Release_Semaphore (Self.Semaphore_Handle, 1, null) then
         return False;
      end if;

      return True;
   end Release;

end GPR2.Build.Jobserver.Semaphore;
