with Ada.Text_IO;
with GPR2.Message; use GPR2.Message;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GPR2.Source_Reference; use GPR2.Source_Reference;

procedure Test is
   procedure Test_Level_Value_Message (Level : Level_Value)
   is
         procedure Test_Message_Display (Message: GPR2.Message.Object)
      is
      begin
         Ada.Text_IO.Put_Line ("* Message content");
         Ada.Text_IO.Put_Line (Message.Message);

         Ada.Text_IO.Put_Line ("* Full path name : True");
         Ada.Text_IO.Put_Line
           (Message.Format (Full_Path_Name => True, Level_Fmt => None));
         Ada.Text_IO.Put_Line
           (Message.Format (Full_Path_Name => True, Level_Fmt => Short));
         Ada.Text_IO.Put_Line
           (Message.Format (Full_Path_Name => True, Level_Fmt => Long));

         Ada.Text_IO.Put_Line ("* Full path name : False");
         Ada.Text_IO.Put_Line
           (Message.Format (Full_Path_Name => False, Level_Fmt => None));
         Ada.Text_IO.Put_Line
           (Message.Format (Full_Path_Name => False, Level_Fmt => Short));
         Ada.Text_IO.Put_Line
           (Message.Format (Full_Path_Name => False, Level_Fmt => Long));
      end Test_Message_Display;

      Message : GPR2.Message.Object;
      Level_Ub : Unbounded_String;
   begin
      case Level is
         when Error => Level_Ub := To_Unbounded_String ("error");
         when End_User => Level_Ub := To_Unbounded_String ("end-user");
         when Warning => Level_Ub := To_Unbounded_String ("warning");
         when Hint => Level_Ub := To_Unbounded_String ("hint");
         when Lint => Level_Ub := To_Unbounded_String ("lint");
      end case;

      Ada.Text_IO.Put_Line ("===  " & To_String (Level_Ub) & " messages");
      Test_Message_Display
        (GPR2.Message.Create
           (Level, To_String (Level_Ub) & " message without sloc nor indent"));
      Test_Message_Display
        (GPR2.Message.Create
          (Level, To_String (Level_Ub) & " message without sloc and with one indent", Indent => 1));
      Test_Message_Display
        (GPR2.Message.Create
          (Level, To_String (Level_Ub) & " message with sloc", GPR2.Source_Reference.Object
             (GPR2.Source_Reference.Create ("path/to/source.ads", 0, 0))));
      Ada.Text_IO.Put_Line ("");

   end Test_Level_Value_Message;

begin
   Test_Level_Value_Message (Error);
   Test_Level_Value_Message (End_User);
   Test_Level_Value_Message (Warning);
   Test_Level_Value_Message (Hint);
   Test_Level_Value_Message (Lint);

   declare
      Sloc : GPR2.Source_Reference.Object :=
        GPR2.Source_Reference.Object
          (GPR2.Source_Reference.Create ("path/to/source.ads", 0, 0));
      Message : GPR2.Message.Object;
   begin

      if Message.Is_Defined then
         Ada.Text_IO.Put_Line ("Message shall be undefined");
      end if;

      Message := GPR2.Message.Create (Error, "message", Sloc);

      if not Message.Is_Defined then
         Ada.Text_IO.Put_Line ("Message shall be defined");
      end if;

      if Message.Sloc /= Sloc then
         Ada.Text_IO.Put_Line ("Failed to obtain the message sloc");
      end if;

      if Message.Status /= Unread then
         Ada.Text_IO.Put_Line ("Status shall be unread");
      end if;

      Message.Set_Status (Read);

      if Message.Status /= Read then
         Ada.Text_IO.Put_Line ("Status shall be read");
      end if;

      if Message.Level /= Error then
         Ada.Text_IO.Put_Line ("Level should be error");
      end if;
   end;
end Test;

