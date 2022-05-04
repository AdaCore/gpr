with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body My_File_Reader is

   Instance : Reader;

   ---------
   -- Add --
   ---------

   procedure Add (Filename : String; Content : Wide_Wide_String) is
   begin
      Instance.Map.Insert
        (GPR2.Path_Name.Create_File (GPR2.Filename_Type (Filename)), Content);
   end Add;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : Reader; Filename : String; Charset : String; Read_BOM : Boolean;
      Contents    : out GPR2.File_Readers.Decoded_File_Contents;
      Diagnostics : in out GPR2.Log.Object)
   is

      procedure Fill_Contents (Buffer : Wide_Wide_String);

      -------------------
      -- Fill_Contents --
      -------------------

      procedure Fill_Contents (Buffer : Wide_Wide_String) is
      begin
         Contents.First := 1;
         Contents.Last := Buffer'Length;
         Contents.Buffer := new Wide_Wide_String (Buffer'First .. Buffer'Last);
         Contents.Buffer.all := Buffer;
      end Fill_Contents;

      File : constant GPR2.Path_Name.Object :=
               GPR2.Path_Name.Create_File (GPR2.Filename_Type (Filename));
   begin
      if Instance.Map.Contains (File) then
         Fill_Contents (Instance.Map.Element (File));
      else
         declare
            F : Ada.Wide_Wide_Text_IO.File_Type;
            C : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
         begin
            Ada.Wide_Wide_Text_IO.Open
              (F, Ada.Wide_Wide_Text_IO.In_File, Filename);
            while not Ada.Wide_Wide_Text_IO.End_Of_File (F) loop
               Ada.Strings.Wide_Wide_Unbounded.Append
                 (Source   => C,
                  New_Item => Ada.Wide_Wide_Text_IO.Get_Line (F));
            end loop;
            Ada.Wide_Wide_Text_IO.Close (F);
            Fill_Contents
              (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (C));
         end;

      end if;
   end Read;

   ---------------
   -- Reference --
   ---------------

   function Reference return GPR2.File_Readers.File_Reader_Reference
   is
   begin
      return GPR2.File_Readers.Create_File_Reader_Reference (Instance);
   end Reference;

end My_File_Reader;
