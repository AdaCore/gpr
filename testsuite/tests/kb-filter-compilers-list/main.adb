with Ada.Text_IO;
with GPR2.KB;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Configuration;
with GPR2; use GPR2;

procedure Main is

   ----------------------------------
   -- Display_Selectable_Compilers --
   ----------------------------------

   procedure Display_Selectable_Compilers (Compilers : GPR2.KB.Compiler_Array) is
   begin

      if Compilers'Length = 0 then
         Ada.Text_IO.Put_Line ("No compilers");
      end if;

      for Compiler of Compilers loop
         if GPR2.KB.Is_Selectable (Compiler) then
            Ada.Text_IO.Put_Line (
               String (GPR2.KB.Name (Compiler))
               & " - " & String (GPR2.Image (GPR2.KB.Language (Compiler)))
               & "," & String (GPR2.KB.Target (Compiler))
               & "," & String (GPR2.KB.Runtime (Compiler)));
            if GPR2.KB.Is_Selected (Compiler) then
               Ada.Text_IO.Put_Line ("^-------- Selected");
            end if;
         end if;

      end loop;
   end Display_Selectable_Compilers;

   package GPC renames GPR2.Project.Configuration;

   Config_Log : Log.Object;
   KB         : GPR2.KB.Object;
   Flags      : GPR2.KB.Parsing_Flags := (True, True, True);

begin

   KB := GPR2.KB.Create (GPR2.KB.Default_Flags);
   KB.Add (Flags, GPR2.Path_Name.Create_File
     ("fake-compiler-description.xml"));
   KB.Add (Flags, GPR2.Path_Name.Create_File ("fake-compiler-config.xml"));

   if KB.Has_Messages then
      for C in KB.Log_Messages.Iterate
         (False, False, True, True, True)
      loop
         declare
            M : constant GPR2.Message.Object := GPR2.Log.Element (C);
         begin
            Ada.Text_IO.Put_Line (M.Format);
         end;
      end loop;
   end if;

   declare
      Descriptions : GPC.Description_Set :=
        (Positive'First => GPC.Create
           (Language => +"Ada",
            Runtime  => "runtime-A",
            Name     => "FAKE-ADA-COMPILER-1"));

      Compilers : GPR2.KB.Compiler_Array := KB.All_Compilers
        (Settings => Descriptions,
         Target   => "all",
         Messages => Config_Log);
   begin
      KB.Filter_Compilers_List (Compilers, "fake-target");
      Display_Selectable_Compilers (Compilers);
   end;

end Main;