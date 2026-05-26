with Ada.Command_Line;              use Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;                   use Ada.Text_IO;
with GNAT.OS_Lib;                   use GNAT.OS_Lib;

procedure Main is
   Gnatbind_Path : String_Access := Locate_Exec_On_Path ("gnatbind");
   
   Args : Argument_List (1 .. Argument_Count);
   
   Success : Boolean;
begin
   if Gnatbind_Path = null then
      Put_Line (Standard_Error, "Error: 'gnatbind' could not be found in your PATH.");
      Os_Exit (1);
   end if;

   -- Populate the argument list with our own arguments
   for I in 1 .. Argument_Count loop
      -- GNAT.OS_Lib expects access strings, so we duplicate the argument strings
      Args (I) := new String'(Argument (I));
   end loop;

   Spawn (Program_Name => Gnatbind_Path.all,
          Args         => Args,
          Success      => Success);

   if not Success then
      Put_Line (Standard_Error, "Error: gnatbind failed to execute properly.");
      Os_Exit (1);
   end if;

   declare
      use Ada.Strings.Unbounded;

      Marker_Begin : constant String := "--  BEGIN Object file/option list";
      Marker_End   : constant String := "--  END Object file/option list";
      L_Prefix     : constant String := "   --   -L";
   
      Max_Lines   : constant := 1024;
      type Line_Array is array (1 .. Max_Lines) of Unbounded_String;
   
      Block_Lines : Line_Array;
      Block_Count : Natural := 0;
   
      Input_File  : Ada.Text_IO.File_Type;
      Output_File : Ada.Text_IO.File_Type;
      Line        : Unbounded_String;
      In_Block    : Boolean := False;
   
      function Starts_With_L (S : String) return Boolean is
      begin
         return S'Length >= L_Prefix'Length
            and then S (S'First .. S'First + L_Prefix'Length - 1) = L_Prefix;
      end Starts_With_L;
   
      --  Find index of the last -L line in the block
      function Last_L_Index return Natural is
      begin
         for I in reverse 1 .. Block_Count loop
            if Starts_With_L (To_String (Block_Lines (I))) then
               return I;
            end if;
         end loop;
         return 0;
      end Last_L_Index;

      function Trim_Right (S : String) return String is
         Last : Integer := S'Last;
      begin
         while Last >= S'First and then S (Last) = ' ' loop
            Last := Last - 1;
         end loop;
         return S (S'First .. Last);
      end Trim_Right;
   begin
      --  Open the original file for reading
      Open (Input_File, In_File, Argument (2));

      --  Create a temporary output file
      Create (Output_File, Out_File, Argument (2)& ".tmp");
   
      while not Ada.Text_IO.End_Of_File (Input_File) loop
         Line := Ada.Strings.Unbounded.Text_IO.Get_Line (Input_File);
   
         if not In_Block then
            Ada.Strings.Unbounded.Text_IO.Put_Line (Output_File, Line);
            if Trim_Right (To_String (Line)) = Marker_Begin then
               In_Block    := True;
               Block_Count := 0;
            end if;
   
         elsif Trim_Right (To_String (Line)) = Marker_End then
            declare
               Last_L : constant Natural := Last_L_Index;
            begin
               for I in 1 .. Block_Count loop
                  declare
                     S : constant String := To_String (Block_Lines (I));
                  begin
                     Ada.Strings.Unbounded.Text_IO.Put_Line (Output_File, Block_Lines (I));
   
                     if I = Last_L then
                        Ada.Text_IO.Put_Line (Output_File, "   --   -Xlinker");
                        Ada.Text_IO.Put_Line (Output_File, "   --   --stack=0x200000,0x1000");
                     elsif Trim_Right (S) = "   --   -static" then
                        Ada.Text_IO.Put_Line (Output_File, "   --   -Xlinker");
                        Ada.Text_IO.Put_Line (Output_File, "   --   --stack=0x800000,0x1000");
                     elsif Trim_Right (S) = "   --   -ldl" then
                        Ada.Text_IO.Put_Line (Output_File, "   --   -Wl,--stack=0x2000000");
                     end if;
                  end;
               end loop;
            end;
   
            Ada.Text_IO.Put_Line (Output_File, To_String (Line));
            In_Block := False;
   
         else
            Block_Count := Block_Count + 1;
            Block_Lines (Block_Count) := Line;
         end if;
      end loop;
   
      Close (Input_File);
      Close (Output_File);

      --  Replace the original with the modified file
      --  (Ada has no built-in rename; use Ada.Directories)
      Ada.Directories.Delete_File (Argument (2));
      Ada.Directories.Rename (Argument (2) & ".tmp", Argument (2));

   exception
      when E : others =>
         --  Ensure files are closed on error
         if Is_Open (Input_File)  then Close (Input_File);  end if;
         if Is_Open (Output_File) then Close (Output_File); end if;
         raise;
   end;

end Main;
