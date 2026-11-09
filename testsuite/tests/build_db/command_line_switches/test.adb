--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.Process;

with GPR2.Build.Command_Line;

with Test_Assert;

function Test return Integer is
   package CL renames GPR2.Build.Command_Line;
   use type GNATCOLL.OS.Process.Argument_List;

   Static : constant String := "-static-libgcc";
   Shared : constant String := "-shared-libgcc";

   procedure Check (First, Last : String);

   procedure Check (First, Last : String) is
      Command  : CL.Object;
      Expected : CL.Object;
   begin
      Command.Add_Argument ("-before", CL.Ignore);
      Command.Add_Argument (First);
      Command.Add_Argument ("path/object.o", CL.Simple, CL.Obj);
      Command.Add_Argument (Last);
      Command.Add_Argument ("-after");

      Expected.Add_Argument ("-before", CL.Ignore);
      Expected.Add_Argument ("path/object.o", CL.Simple, CL.Obj);
      Expected.Add_Argument (Last);
      Expected.Add_Argument ("-after");

      Command.Filter_Conflicting_Switches (Static, Shared);
      Test_Assert.Assert (Command.Argument_List = Expected.Argument_List);
      Test_Assert.Assert (Command.Total_Length, Expected.Total_Length);
      Test_Assert.Assert (Command.Signature, "object.o " & Last & " -after");
      Test_Assert.Assert
        (Command.Argument_List (CL.Obj) = Expected.Argument_List (CL.Obj));

      Command.Filter_Conflicting_Switches (Static, Shared);
      Test_Assert.Assert (Command.Argument_List = Expected.Argument_List);
      Test_Assert.Assert (Command.Signature = Expected.Signature);
   end Check;

   Command : CL.Object;
begin
   Command.Filter_Conflicting_Switches (Static, Shared);
   Test_Assert.Assert (Command.Argument_List.Is_Empty);
   Test_Assert.Assert (Command.Total_Length, 0);
   Test_Assert.Assert (Command.Signature, "");

   Check (Static, Shared);
   Check (Shared, Static);
   Check (Static, Static);
   Check (Shared, Shared);

   Command.Add_Argument ("-static-libgcc=other");
   Command.Add_Argument ("-shared-libgcc-extra");
   Command.Add_Argument ("literal -static-libgcc");
   declare
      Before : constant GNATCOLL.OS.Process.Argument_List :=
                 Command.Argument_List;
      Length : constant Natural := Command.Total_Length;
   begin
      Command.Filter_Conflicting_Switches (Static, Shared);
      Test_Assert.Assert (Command.Argument_List = Before);
      Test_Assert.Assert (Command.Total_Length, Length);
      Test_Assert.Assert
        (Command.Signature,
         "-static-libgcc=other -shared-libgcc-extra literal -static-libgcc");
   end;

   return Test_Assert.Report;
end Test;
