--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--


with Ada.Unchecked_Conversion;

with GNATCOLL.OS.FS;   use GNATCOLL.OS.FS;
with GNATCOLL.OS.Stat; use GNATCOLL.OS.Stat;
with GNATCOLL.Traces;

package body GPR2.Build.Jobserver_Protocol.Pipe is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create
                ("GPR.BUILD.JOBSERVER.PROTOCOL.PIPE",
                 GNATCOLL.Traces.Off);

   procedure Check (Self : in out Object);
   procedure Writec is new Write_Bytes (T => Character);

   -----------
   -- Check --
   -----------

   procedure Check (Self : in out Object) is
   begin
      if Self.Read_FD = Invalid_FD or else Self.Write_FD = Invalid_FD then
         Traces.Trace ("Pipe protocol: invalid FDs received");
         Self := (others => <>);
      end if;

      --  Make sure we opened a pipe, not a simple file
      if Is_File (Fstat (Self.Read_FD))
        or else Is_File (Fstat (Self.Write_FD))
      then
         Traces.Trace ("Pipe protocol: FDs are files, not pipes");
         Close (Self.Read_FD);
         Close (Self.Write_FD);
         Self := (others => <>);
      end if;
   end Check;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Object) is
   begin
      if Self.Read_FD /= Invalid_FD then
         Close (Self.Read_FD);
         Self.Read_FD := Invalid_FD;
      end if;

      if Self.Write_FD /= Invalid_FD then
         Close (Self.Write_FD);
         Self.Write_FD := Invalid_FD;
      end if;
   end Finalize;

   ---------------
   -- Get_Token --
   ---------------

   overriding function Get_Token
     (Self  : Object;
      Token : out Character) return Boolean is
   begin
      return Unsafe_Read (Self.Read_FD, Token'Address, 1) = 1;
   exception
      when others =>
         return False;
   end Get_Token;

   ----------------
   -- Initialize --
   ----------------

   overriding function Initialize (Param : String) return Object
   is
      Result : Object;
   begin
      Traces.Trace ("opening a named pipe protocol for " & Param);
      Result.Read_FD  := GNATCOLL.OS.FS.Open (Param, Read_Mode);
      Result.Write_FD := GNATCOLL.OS.FS.Open (Param, Write_Mode);

      Check (Result);

      return Result;
   end Initialize;

   function Initialize (R, W : Integer) return Object is
      function To_Fd is new
        Ada.Unchecked_Conversion (Integer, File_Descriptor);

      Result : Object;
   begin
      if R <= 0 or else W <= 0 then
         Traces.Trace ("invalid FDs received" & R'Image & W'Image);
         return (others => <>);
      end if;

      Traces.Trace ("opening a simple pipe protocol for" & R'Image & W'Image);
      Result.Read_FD := To_Fd (R);
      Result.Write_FD := To_Fd (W);

      Check (Result);

      return Result;
   end Initialize;

   ------------------
   -- Is_Available --
   ------------------

   overriding function Is_Available (Self : Object) return Boolean is
   begin
      return Self.Read_FD /= Invalid_FD
        and then Self.Write_FD /= Invalid_FD;
   end Is_Available;

   -------------------
   -- Release_Token --
   -------------------

   overriding procedure Release_Token
     (Self  : Object;
      Token : Character) is
   begin
      Writec (Self.Write_FD, Token);
   end Release_Token;

end GPR2.Build.Jobserver_Protocol.Pipe;
