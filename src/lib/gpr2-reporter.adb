--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  Reports log messages

package body GPR2.Reporter is



   ------------
   -- Report --
   ------------

   procedure Report
     (Self           : Object'Class;
      Messages       : GPR2.Log.Object;
      Warn_If_Errors : Boolean := False) is
   begin
      if Self.Verbosity = Quiet then
         return;
      end if;

      --  If the log contains errors, then only these errors are displayed.
      for C in Messages.Iterate
        (Information => Self.Verbosity >= Verbose,
         Warning     => Self.Verbosity > No_Warnings
                          and then (Warn_If_Errors
                                      or else not Messages.Has_Error),
         Error       => True,
         Lint        => Self.Verbosity >= Verbose)
      loop
         Self.Internal_Report (GPR2.Log.Element (C));
      end loop;
   end Report;

   procedure Report (Self : Object'Class; Message : GPR2.Message.Object)
   is

      function Printable (Severity : GPR2.Message.Level_Value) return Boolean;
   --  Returns True if the reporter's verbosity is sufficient to print a
   --  message with the specified severity.

      ---------------
      -- Printable --
      ---------------

      function Printable (Severity : GPR2.Message.Level_Value) return Boolean
      is
         use all type GPR2.Message.Level_Value;
      begin
         case Severity is
            when Error =>
               return Self.Verbosity >= No_Warnings;
            when Warning =>
               return Self.Verbosity > No_Warnings;
            when Information | Lint =>
               return Self.Verbosity >= Verbose;

         end case;
      end Printable;
   begin
      if Printable (Message.Level) then
         Self.Internal_Report (Message);
      end if;
   end Report;

   procedure Report (Self : Object'Class; Message : String)
   is
   begin
      if Self.Verbosity > Quiet then
         Self.Internal_Report (Message);
      end if;
   end Report;
end GPR2.Reporter;
