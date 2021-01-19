with Ada.Text_IO; use Ada.Text_IO;

with GPR2.KB; use GPR2.KB;
with GPR2.Message; use GPR2.Message;
with GPR2.Log;
with GPR2.Path_Name; use GPR2.Path_Name;

procedure Main is
   Base1, Base2, Base3, Base4 : GPR2.KB.Object;
   Msg                        : GPR2.Message.Object;
   Log                        : GPR2.Log.Object;
   Flags                      : Parsing_Flags := (True, True, True);

   Valid_Chunk : constant String :=
                   "<gprconfig>"
                   & "   <configuration>"
                   & "      <config>"
                   & "         --  Foo"
                   & "      </config>"
                   & "   </configuration>"
                   & "</gprconfig>";


   Invalid_Chunk : constant String :=
                     "<gprconfig>"
                     & "   <configuration2>"
                     & "   </configuration2>"
                     & "</gprconfig>";
begin

   Base1 := Create (Default_Location, Flags);
   Base1.Add (Flags, Valid_Chunk);
   Base1.Add (Flags, GPR2.Path_Name.Create_File ("valid.xml"));
   Base1.Add (Flags, Invalid_Chunk);
   for Msg in Base1.Log_Messages.Iterate
     (Information => False, Warning => False)
   loop
      Put_Line (GPR2.Log.Element (Msg).Format);
   end loop;

   Base2 := Create (Default_Location, Flags);
   Base2.Add (Flags, Valid_Chunk);
   Base2.Add (Flags, GPR2.Path_Name.Create_File ("valid.xml"));
   Base2.Add (Flags, GPR2.Path_Name.Create_File ("invalid.xml"));

   for Msg in Base2.Log_Messages.Iterate
     (Information => False, Warning => False)
   loop
      Put_Line (GPR2.Log.Element (Msg).Format);
   end loop;

   Base3 := Create_Default (Flags);
   Base3.Add (Flags, Valid_Chunk);
   Base3.Add (Flags, GPR2.Path_Name.Create_File ("valid.xml"));
   Base3.Add (Flags, Invalid_Chunk);
   for Msg in Base3.Log_Messages.Iterate
     (Information => False, Warning => False)
   loop
      Put_Line (GPR2.Log.Element (Msg).Format);
   end loop;

   Base4 := Create_Default (Flags);
   Base4.Add (Flags, Valid_Chunk);
   Base4.Add (Flags, GPR2.Path_Name.Create_File ("valid.xml"));
   Base4.Add (Flags, GPR2.Path_Name.Create_File ("invalid.xml"));

   for Msg in Base2.Log_Messages.Iterate
     (Information => False, Warning => False)
   loop
      Put_Line (GPR2.Log.Element (Msg).Format);
   end loop;

end Main;
