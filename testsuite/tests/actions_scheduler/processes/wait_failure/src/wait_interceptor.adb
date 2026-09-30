--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Task_Identification;
with Ada.Text_IO;
package body Wait_Interceptor is
   use type Ada.Task_Identification.Task_Id;
   use type GNATCOLL.OS.Process.Process_State;
   Raised : Boolean := False;
   function Real_Wait
     (Handle : GNATCOLL.OS.Process.Process_Handle) return Integer
     with Import, Convention => C,
          External_Name => "__real_gnatcoll__os__process__wait";
   function Wait (Handle : GNATCOLL.OS.Process.Process_Handle) return Integer is
   begin
      if not Raised and then Ada.Task_Identification.Current_Task /=
        Ada.Task_Identification.Environment_Task
      then
         Raised := True;
         Ada.Text_IO.Put_Line ("injected wait failure");
         raise Constraint_Error with "injected wait failure";
      end if;
      declare
         Result : constant Integer := Real_Wait (Handle);
      begin
         if GNATCOLL.OS.Process.State (Handle) /=
           GNATCOLL.OS.Process.TERMINATED
         then
            raise Program_Error with "child was not reaped";
         end if;
         Ada.Text_IO.Put_Line ("child reaped");
         return Result;
      end;
   end Wait;
end Wait_Interceptor;
