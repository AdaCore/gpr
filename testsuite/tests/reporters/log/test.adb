with Ada.Text_IO;
with GPR2.Reporter; use GPR2.Reporter;
with GPR2.Message; use GPR2.Message;
with GPR2.Reporter.Log;
with GPR2.Log;

procedure Test is
   procedure Test_Messages_And_Logs (Reporter : in out GPR2.Reporter.Log.Object)
   is

      procedure Print_Log (Log : GPR2.Log.Object)
      is
      begin
         for C in Reporter.Log.Iterate
           (Error    => True,
            End_User => True,
            Warning  => True,
            Hint     => True,
            Lint     => True,
            Read     => True,
            Unread   => True)
         loop
            declare
               M : constant GPR2.Message.Object := GPR2.Log.Element (C);
            begin
               Ada.Text_IO.Put_Line (M.Format);
            end;
         end loop;
      end Print_Log;

      Log : GPR2.Log.Object;
   begin
      Ada.Text_IO.Put_Line (" * Single message reporting:");

      Reporter.Report (GPR2.Message.Create (Error, "An single error message"));
      Reporter.Report (GPR2.Message.Create (End_User, "An end user message"));
      Reporter.Report ("A simple string");
      Reporter.Report (GPR2.Message.Create (Warning, "A warning message"));
      Reporter.Report (GPR2.Message.Create (Hint, "A hint message"));
      Reporter.Report (GPR2.Message.Create (Lint, "A lint message"));

      Print_Log (Reporter.Log);
      Reporter.Clear_Log;

      Ada.Text_IO.Put_Line (" * Single error message + Log without error messages:");

      Reporter.Report (GPR2.Message.Create (Error, "A single error message"));
      Log.Append (GPR2.Message.Create (End_User, "An end user message"));
      Log.Append (GPR2.Message.Create (Warning, "A warning message"));
      Log.Append (GPR2.Message.Create (Hint, "A hint message"));
      Log.Append (GPR2.Message.Create (Lint, "A lint message"));

      Reporter.Report (Log);
      Print_Log (Reporter.Log);
      Reporter.Clear_Log;
      Log.Clear;

      Ada.Text_IO.Put_Line (" * Single error message + Log with error messages:");

      Reporter.Report (GPR2.Message.Create (Error, "A single error message"));
      Log.Append (GPR2.Message.Create (Error, "An error message"));
      Log.Append (GPR2.Message.Create (End_User, "An end user message"));
      Log.Append (GPR2.Message.Create (Warning, "A warning message"));
      Log.Append (GPR2.Message.Create (Hint, "A hint message"));
      Log.Append (GPR2.Message.Create (Lint, "A lint message"));

      Reporter.Report (Log);
      Print_Log (Reporter.Log);
      Reporter.Clear_Log;
      Log.Clear;

      Ada.Text_IO.Put_Line
        (" * Single message + Log with error messages and Warn_If_Errors set:");

      Reporter.Report (GPR2.Message.Create (Error, "A single error message"));
      Log.Append (GPR2.Message.Create (Error, "An error message"));
      Log.Append (GPR2.Message.Create (End_User, "An end user message"));
      Log.Append (GPR2.Message.Create (Warning, "A warning message"));
      Log.Append (GPR2.Message.Create (Hint, "A hint message"));
      Log.Append (GPR2.Message.Create (Lint, "A lint message"));

      Reporter.Report (Log, Warn_If_Errors => True);
      Print_Log (Reporter.Log);
      Reporter.Clear_Log;
      Log.Clear;

      Ada.Text_IO.Put_Line ("");

   end Test_Messages_And_Logs;

   Reporter : GPR2.Reporter.Log.Object := GPR2.Reporter.Log.Create;
begin
   Ada.Text_IO.Put_Line ("=== Verbosity : Regular ===");

   if Reporter.Verbosity /= Regular then
      Ada.Text_IO.Put_Line ("Invalid reporter verbosity");

      return;
   end if;

   Test_Messages_And_Logs (Reporter);

   Ada.Text_IO.Put_Line ("=== Verbosity : Quiet ===");
   Reporter.Set_Verbosity (Quiet);

   if Reporter.Verbosity /= Quiet then
      Ada.Text_IO.Put_Line ("Invalid reporter verbosity");

      return;
   end if;

   Test_Messages_And_Logs (Reporter);

   Ada.Text_IO.Put_Line ("=== Verbosity : No_Warnings ===");
   Reporter.Set_Verbosity (No_Warnings);

   if Reporter.Verbosity /= No_Warnings then
      Ada.Text_IO.Put_Line ("Invalid reporter verbosity");

      return;
   end if;

   Test_Messages_And_Logs (Reporter);

   Ada.Text_IO.Put_Line ("=== Verbosity : Verbose ===");
   Reporter.Set_Verbosity (Verbose);

   if Reporter.Verbosity /= Verbose then
      Ada.Text_IO.Put_Line ("Invalid reporter verbosity");

      return;
   end if;

   Test_Messages_And_Logs (Reporter);

   Ada.Text_IO.Put_Line ("=== Verbosity : Very verbose ===");
   Reporter.Set_Verbosity (Very_Verbose);

   if Reporter.Verbosity /= Very_Verbose then
      Ada.Text_IO.Put_Line ("Invalid reporter verbosity");

      return;
   end if;

   Test_Messages_And_Logs (Reporter);
end Test;

