--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Exceptions;

package body GPR2.Build.Jobserver.Pipe is

   ------------
   -- Create --
   ------------

   function Create (R, W : GOF.File_Descriptor) return Object
   is
      JS : Object;
   begin
      JS.Kind := JM_Simple_Pipe;
      JS.Traces.Trace ("Connection method : " & JS.Kind'Img);
      JS.Read_FD    := R;
      JS.Write_FD   := W;
      return JS;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (RW : String) return Object
   is
      JS : Object;
   begin
      JS.Kind := JM_Named_Pipe;
      JS.Traces.Trace ("Connection method : " & JS.Kind'Img);
      JS.Read_FD    := GOF.Open (RW, GOF.Read_Mode);
      JS.Write_FD   := GOF.Open (RW, GOF.Write_Mode);
      return JS;
   end Create;

   ------------------
   -- Is_Integrous --
   ------------------

   overriding function Is_Integrous (Self : Object) return Boolean is
   begin
      --  We had some cases where during execution of gpr1build the file
      --  descriptors were overriden and this ended up in exceptions, this
      --  was only happening on Simple_Pipe jobserver kind and only on versions
      --  anterior to a certain point.
      if Self.Kind = JM_Simple_Pipe then
         if GOS.Is_File (GOS.Fstat (Self.Read_FD))
           or else GOS.Is_File (GOS.Fstat (Self.Write_FD))
         then
            return False;
         end if;
      end if;
      return True;
   end Is_Integrous;

   --------------
   -- Register --
   --------------

   overriding function Register
     (Self : Object;
      Char : out Character) return Boolean is
   begin
      if GOF.Unsafe_Read (Self.Read_FD, Char'Address, 1) /= 1 then
         return False;
      end if;
      return True;
   exception
      when E : others =>
         Self.Traces.Trace
           ("Exception : " & Ada.Exceptions.Exception_Message (E));
         return False;
   end Register;

   -------------
   -- Release --
   -------------

   overriding function Release
     (Self : Object;
      Char : Character) return Boolean
   is
      procedure Write is new GOF.Write_Bytes (T => Character);
   begin
      Write (Self.Write_FD, Char);
      return True;
   exception
      when E : others =>
         Self.Traces.Trace
           ("Exception : " & Ada.Exceptions.Exception_Message (E));
         return False;
   end Release;

end GPR2.Build.Jobserver.Pipe;
