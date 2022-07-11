with Ada.IO_Exceptions;
with Ada.Streams;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree.View_Builder;

with Interfaces.C;

procedure Main is

   type Stdio_Stream is new Ada.Streams.Root_Stream_Type with null record;
   --  Stream that reads and writes standard input/output descriptors

   overriding procedure Read
     (Stream : in out Stdio_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Stdio_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

   package C renames Interfaces.C;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Stdio_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Stream);
      use type Ada.Streams.Stream_Element_Offset;

      function read
        (fildes : C.int;
         buf    : out Ada.Streams.Stream_Element_Array;
         nbyte  : C.size_t) return C.size_t
        with Import => True,
        Convention => C,
        External_Name => "read";
      Done    : C.size_t;
   begin
      Done := read (0, Item, Item'Length);
      Last := Item'First + Ada.Streams.Stream_Element_Offset (Done) - 1;

      if Last < Item'First then
         raise Ada.IO_Exceptions.End_Error with "end of file";
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Stdio_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      function write
        (fildes : C.int;
         buf    : Ada.Streams.Stream_Element_Array;
         nbyte  : C.size_t) return C.size_t
        with Import => True,
        Convention => C,
        External_Name => "write";
      pragma Unreferenced (Stream);

      Ignore : C.size_t := write (1, Item, Item'Length);
   begin
      null;
   end Write;

   --  The input task
   task type Input_Task_Type
   is
      entry Start;
      --  Start the task. Should be called once.
   end Input_Task_Type;

   Buffer_Size : constant := 512;
   Vector        : Ada.Strings.Unbounded.Unbounded_String;
   Buffer : Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);
   Last   : Ada.Streams.Stream_Element_Count :=
   Ada.Streams.Stream_Element_Count
       (Ada.Strings.Unbounded.Length (Vector));
   Stream : aliased Stdio_Stream;

   task body Input_Task_Type is
   begin
      accept Start;
      while True loop
         Stream.Read (Buffer, Last);
      end loop;
   end Input_Task_Type;

   Input_Task : Input_Task_Type;

   Tree : GPR2.Project.Tree.Object;
   Context : GPR2.Context.Object;
   Root : constant GPR2.Project.Tree.View_Builder.Object :=
     GPR2.Project.Tree.View_Builder.Create
       (GPR2.Path_Name.Create_Directory ("demo"), "Custom_Project");

begin
   Input_Task.Start;
   delay 0.1;

   GPR2.Project.Tree.View_Builder.Load_Autoconf (Tree, Root, Context);
   if Tree.Log_Messages.Has_Error then
      Tree.Log_Messages.Output_Messages;
   end if;

   Tree.Unload;

   Tree.Load_Autoconf (GPR2.Path_Name.Create_File ("test.gpr"), Context);

   if Tree.Log_Messages.Has_Error then
      Tree.Log_Messages.Output_Messages;
   end if;

   GNAT.OS_Lib.OS_Exit (0);
exception
   when others =>
      if Tree.Log_Messages.Has_Error then
         Tree.Log_Messages.Output_Messages;
      end if;
end Main;
