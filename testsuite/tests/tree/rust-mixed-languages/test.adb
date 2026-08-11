--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GPR2;
with GPR2.Log;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Reporter.Log;

with Test_Assert; use Test_Assert;

function Test return Integer is
   Tree     : GPR2.Project.Tree.Object;
   Options  : GPR2.Options.Object;
   Reporter : GPR2.Reporter.Log.Object := GPR2.Reporter.Log.Create;
   --  A log reporter so messages are captured rather than printed

   Expected : constant String :=
                "language Rust cannot be combined with other languages";

   Found    : Boolean := False;
   Errors   : Unbounded_String;

begin
   Options.Add_Switch (GPR2.Options.P, "mixed.gpr");

   --  Cargo drives the whole build of a Rust project, so it cannot produce
   --  the artifacts of another language. A project declaring Rust together
   --  with Ada must be rejected when the tree is loaded, rather than have
   --  its Ada sources silently left unbuilt.

   Assert (not Tree.Load (Options, Reporter => Reporter),
           "loading a project mixing Rust and Ada must fail");

   for C in Tree.Log_Messages.all.Iterate
              (Error    => True,
               Warning  => False,
               End_User => False,
               Hint     => False,
               Lint     => False)
   loop
      declare
         Message : constant String := GPR2.Log.Element (C).Format;
      begin
         Append (Errors, ASCII.LF & Message);

         if Ada.Strings.Fixed.Index (Message, Expected) > 0 then
            Found := True;
         end if;
      end;
   end loop;

   Assert (Found,
           "expected an error containing """ & Expected & """, got:"
           & To_String (Errors));

   return Report;
end Test;
