with Unchecked_Deallocation;
with GNAT.Debug_Pools;
with System;

package body Test is

   Test_Ended : Boolean := False;
   Test_OK    : Boolean := True;

   type T is access Integer;

   P : GNAT.Debug_Pools.Debug_Pool;
   for T'Storage_Pool use P;

   procedure Free is new Unchecked_Deallocation (Integer, T);

   task type Test_Task is
      pragma Priority (System.Max_Priority);
      entry Start_Task (Id : Integer);
   end Test_Task;
   type Test_Task_Access is access Test_Task;
   Number_Of_Task : constant Integer := 2;
   Test_Tasks     : array (1 .. Number_Of_Task) of Test_Task_Access;
   Indexes        : array (1 .. Number_Of_Task) of Long_Long_Integer;
   pragma Volatile (Indexes);

   MAX : constant := 1000;
   TOP : constant := 100000;
   
   procedure Update_Test_Ended is
      End_Test : Boolean := True;
   begin
      for I in Test_Tasks'Range loop
         if (Indexes (I) < MAX) then
            End_Test := False;
         end if;
      end loop;

      if End_Test then
         Test_Ended := True;
      end if;
   end Update_Test_Ended;

   
   task body Test_Task is
      Index : Integer;
   begin
      accept Start_Task (Id : Integer) do
         Index := Id;
      end Start_Task;
      while not Test_Ended and then Indexes (Index)  < TOP loop

         declare
            A : aliased T;
            I : Integer;
         begin
            A     := new Integer;
            A.all := Index;
            I     := A.all;
            A.all := I;
            Free (A);
            if Indexes (Index) >= MAX then
               delay 0.0;
               Update_Test_Ended;
            end if;

            Indexes (Index) := Indexes (Index) + 1;
         end;
      end loop;
      Update_Test_Ended;
   exception
      when others =>
         Test_Ended    := True;
         Test_OK       := False;
         Indexes (Index) := MAX;

   end Test_Task;

   -----------------
   -- Start_Tasks --
   -----------------

   procedure Start_Tasks is
   begin
      Test_Ended := False;
      for I in Test_Tasks'Range loop
         Indexes (I)    := 0;
         Test_Tasks (I) := new Test_Task;
      end loop;
      for I in Test_Tasks'Range loop
         Test_Tasks (I).Start_Task (I);
      end loop;
   end Start_Tasks;

   function Is_Test_Ended return Boolean is
   begin
      return Test_Ended;
   end Is_Test_Ended;

   function Is_Test_OK return Boolean is
   begin
      return Test_OK;
   end Is_Test_OK;

end Test;
