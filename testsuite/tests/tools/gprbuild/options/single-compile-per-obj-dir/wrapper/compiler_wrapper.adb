with Ada.Calendar;              use Ada.Calendar;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Directories;           use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Text_IO;               use Ada.Text_IO;

with GNAT.OS_Lib;

function Compiler_Wrapper return Integer is
   Events   : constant String := Value ("SINGLE_COMPILE_EVENTS");
   Serial   : constant Boolean := Value ("SINGLE_COMPILE_SERIAL") = "1";
   Parallel : constant Positive :=
     Positive'Value (Value ("SINGLE_COMPILE_PARALLEL"));
   Lock     : constant String := "compilation.lock";
   Locked   : Boolean := False;
   Compile  : Boolean := False;
   Source   : Natural := 0;
   Log      : File_Type;
   Result   : Integer;

   function Timestamp return String is
     (Duration'Image (Clock - Time_Of (2000, 1, 1)));
   --  Comparable timestamps across wrapper processes, in seconds

   function Run_Compiler return Integer is
      Args   : GNAT.OS_Lib.Argument_List (1 .. Argument_Count);
      Status : Integer;
   begin
      --  Forward the original arguments unchanged to the real compiler.
      for J in Args'Range loop
         Args (J) := new String'(Argument (J));
      end loop;
      Status := GNAT.OS_Lib.Spawn (Value ("SINGLE_COMPILE_CC"), Args);
      for Arg of Args loop
         GNAT.OS_Lib.Free (Arg);
      end loop;
      return Status;
   end Run_Compiler;

   function Try_Create (Directory : String) return Boolean is
   begin
      --  Creation is atomic: False means another compiler got there first.
      --  We detect overlap without making a compiler wait for a lock.
      Create_Directory (Directory);
      return True;
   exception
      when Ada.IO_Exceptions.Use_Error =>
         if Ada.Directories.Exists (Directory) then
            return False;
         end if;
         raise;
   end Try_Create;

   function Started_Compilations return Natural is
      Search : Search_Type;
      Item   : Directory_Entry_Type;
      Count  : Natural := 0;
   begin
      Start_Search (Search, Events, "*.log",
                    (Ordinary_File => True, others => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Item);
         Count := Count + 1;
      end loop;
      End_Search (Search);
      return Count;
   end Started_Compilations;

begin
   for J in 1 .. Argument_Count loop
      declare
         Arg : constant String := Argument (J);
      begin
         Compile := Compile or else Arg = "-c";
         if Tail (Arg, 2) = ".c" or else Tail (Arg, 4) in ".adb" | ".ads" then
            Source := J;
         end if;
      end;
   end loop;

   if not Compile or else Source = 0 then
      return Run_Compiler;
   end if;

   --  Reject overlapping compilations in the current object directory.
   if Serial then
      Locked := Try_Create (Lock);
      if not Locked then
         Put_Line (Standard_Error, "simultaneous compilations in "
                   & Current_Directory);
         return 1;
      end if;
   end if;

   --  One log per source, containing its compilation start and finish times.
   Create (Log, Out_File,
           Compose (Events, Simple_Name (Argument (Source)) & ".log"));
   Put_Line (Log, Timestamp);
   Flush (Log);

   --  A parallelism check waits for a second compiler to start. Globally
   --  serializing the build must time out here instead of passing.
   for Attempt in 1 .. 3_000 loop
      exit when Started_Compilations >= Parallel;
      delay 0.01;
   end loop;

   --  Keep the compiler active long enough to detect overlap reliably.
   delay 0.1;
   if Started_Compilations < Parallel then
      Put_Line (Standard_Error, "independent compilations did not run in parallel");
      Result := 1;
   elsif Value ("SINGLE_COMPILE_FAIL_FIRST") = "1"
     and then Try_Create (Compose (Events, "failed"))
   then
      Put_Line (Standard_Error, "intentional compiler failure");
      Result := 1;
   else
      Result := Run_Compiler;
   end if;

   Put_Line (Log, Timestamp);
   Close (Log);
   if Locked then
      Delete_Directory (Lock);
   end if;
   return Result;
end Compiler_Wrapper;
