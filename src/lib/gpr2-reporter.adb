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
      if Self.Verbosity = Quiet and then Self.User_Verbosity = Quiet then
         return;
      end if;

      for C in Messages.Iterate
        (Error       => Self.Verbosity > Quiet,
         End_User    => (Self.User_Verbosity = Unset
                         and then Self.Verbosity > Quiet)
                           or else Self.User_Verbosity > Quiet,
         Warning     => Self.Verbosity > No_Warnings
                          and then (Warn_If_Errors
                                      or else not Messages.Has_Error),
         Hint        => Self.Verbosity >= Verbose,
         Lint        => Self.Verbosity >= Verbose)
      loop
         declare
            Msg : Message.Object renames GPR2.Log.Element (C);
            use GPR2.Message;
         begin
            if Msg.Level /= End_User
              or else Msg.User_Level = Regular
              or else Self.User_Verbosity = Verbose
            then
               Self.Internal_Report (GPR2.Log.Element (C));
            end if;
         end;
      end loop;
   end Report;

   procedure Report
     (Self      : in out Object'Class;
      Message   : GPR2.Message.Object)
   is

      function Printable
        (Severity      : GPR2.Message.Level_Value;
         User_Severity : GPR2.Message.User_Level_Value) return Boolean;
   --  Returns True if the reporter's verbosity is sufficient to print a
   --  message with the specified severity.

      ---------------
      -- Printable --
      ---------------

      function Printable
        (Severity      : GPR2.Message.Level_Value;
         User_Severity : GPR2.Message.User_Level_Value) return Boolean
      is
         use all type GPR2.Message.Level_Value;
         use type GPR2.Message.User_Level_Value;
      begin
         case Severity is
            when Error =>
               return Self.Verbosity > Quiet;

            when End_User =>
               case Self.User_Verbosity is
                  when Unset =>
                     return Self.Verbosity > Quiet;
                  when Quiet =>
                     --  ??? We would still need a fully quiet mode?
                     return User_Severity = GPR2.Message.Important;
                  when Regular =>
                     return User_Severity /= GPR2.Message.Optional;
                  when Verbose =>
                     return True;
               end case;

            when Warning =>
               return Self.Verbosity > No_Warnings;

            when Hint | Lint =>
               return Self.Verbosity >= Verbose;

         end case;
      end Printable;

   begin
      if Printable (Message.Level, Message.User_Level) then
         Self.Internal_Report (Message);
      end if;
   end Report;

   procedure Report
     (Self      : in out Object'Class;
      Message   : String;
      To_Stderr : Boolean := False;
      Level     : GPR2.Message.User_Level_Value := GPR2.Message.Regular)
   is
      use all type GPR2.Message.Level_Value;
      Msg : constant GPR2.Message.Object :=
              GPR2.Message.Create
                (Level      => End_User,
                 Message    => Message,
                 To_Stderr  => To_Stderr,
                 User_Level => Level);
   begin
      Self.Report (Msg);
   end Report;

end GPR2.Reporter;
