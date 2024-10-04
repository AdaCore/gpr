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
     (Self           : in out Object'Class;
      Messages       : GPR2.Log.Object;
      Warn_If_Errors : Boolean := False) is
   begin
      if Self.Verbosity = Quiet then
         return;
      end if;

      for C in Messages.Iterate
        (Error       => True,
         End_User    => Self.Verbosity >= No_Warnings,
         Warning     => Self.Verbosity > No_Warnings
                          and then (Warn_If_Errors
                                      or else not Messages.Has_Error),
         Hint        => Self.Verbosity >= Verbose,
         Lint        => Self.Verbosity >= Verbose)
      loop
         Self.Internal_Report (GPR2.Log.Element (C));
      end loop;
   end Report;

   procedure Report
     (Self      : in out Object'Class;
      Message   : GPR2.Message.Object)
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
            when Error | End_User =>
               return Self.Verbosity >= No_Warnings;
            when Warning =>
               return Self.Verbosity > No_Warnings;
            when Hint | Lint =>
               return Self.Verbosity >= Verbose;

         end case;
      end Printable;

   begin
      if Printable (Message.Level) then
         Self.Internal_Report (Message);
      end if;
   end Report;

   procedure Report
     (Self    : in out Object'Class;
      Message : String;
      To_Stderr : Boolean := False)
   is
      use all type GPR2.Message.Level_Value;
      Msg : constant GPR2.Message.Object :=
              GPR2.Message.Create
                (Level     => End_User,
                 Message   => Message,
                 To_Stderr => To_Stderr);
   begin
      Self.Report (Msg);
   end Report;

end GPR2.Reporter;
