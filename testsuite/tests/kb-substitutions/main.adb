--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;
with GPR2.KB;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Configuration;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GPR2; use GPR2;

procedure Main is

   package GPC renames GPR2.Project.Configuration;

   type Unbounded_String_Array is
     array (Positive range <>) of Unbounded_String;

   procedure Test
     (Test_Title            : String;
      KB                    : in out GPR2.KB.Object;
      Descriptions          : GPC.Description_Set;
      Expected_String_Parts : Unbounded_String_Array);
   --  Filter compilers for the "fake-target" target and
   --  obtain the KB configuration.
   --  Display errors and warnings if presents.
   --  If Expected_String_Parts is not empty, check that
   --  all the strings are in the configuration.
   --  If the configuration is correct, displays OK.
   --  Otherwise, displays KO.

   ----------
   -- Test --
   ----------

   procedure Test
     (Test_Title            : String;
      KB                    : in out GPR2.KB.Object;
      Descriptions          : GPC.Description_Set;
      Expected_String_Parts : Unbounded_String_Array)
   is
      Config_Log : Log.Object;
      Compilers : GPR2.KB.Compiler_Array := KB.All_Compilers
        (Settings => Descriptions,
         Target   => "fake-target",
         Messages => Config_Log);
   begin
      KB.Filter_Compilers_List (Compilers, "fake-target");
      declare
         Conf : String := To_String (KB.Configuration
                      (Selection => Compilers,
                       Target    => "fake-target",
                       Messages  => Config_Log));
         Result : Boolean := True;
      begin

         Ada.Text_IO.Put_Line ("=== " & Test_Title & " ===");

         if Config_Log.Has_Error then
            Result := False;
         end if;


         for C in Config_Log.Iterate
            (True, True, True, True, True)
         loop
            declare
               M : constant GPR2.Message.Object := GPR2.Log.Element (C);
            begin
               Ada.Text_IO.Put_Line (M.Format);
            end;
         end loop;

         if Result then
            for Expected_Str of Expected_String_Parts loop
               if Ada.Strings.Fixed.Index
                    (Conf, To_String (Expected_Str)) <= 0 then
                  Result := False;
               end if;
            end loop;
         end if;

         if Result then
            Ada.Text_IO.Put_Line ("OK");
         else
            Ada.Text_IO.Put_Line ("KO");
         end if;
         Ada.Text_IO.Put_LIne ("");

      end;
   end Test;

   KB         : GPR2.KB.Object;
   Flags      : GPR2.KB.Parsing_Flags := (True, True, True);

begin

   KB := GPR2.KB.Create (GPR2.KB.Default_Flags);
   KB.Add (Flags, GPR2.Path_Name.Create_File
                    ("fake-compiler-description.xml"));
   KB.Add (Flags, GPR2.Path_Name.Create_File
                    ("fake-compiler-config.xml"));

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
      Expected_Strings : Unbounded_String_Array :=
        (To_Unbounded_String ("Gprconfig_Prefix"),
         To_Unbounded_String ("Target := fake-target"));
   begin
      Test ("Prefined variable", KB, Descriptions, Expected_Strings);
   end;

   declare
      Descriptions : GPC.Description_Set :=
        (GPC.Create
           (Language => +"Ada",
            Runtime  => "runtime-A",
            Name     => "FAKE-ADA-COMPILER-1"),
         GPC.Create
           (Language => +"C",
            Runtime  => "runtime-A",
            Name     => "FAKE-C-COMPILER-1"));
      Expected_Strings : Unbounded_String_Array :=
        (To_Unbounded_String ("Gprconfig_Prefix"),
         To_Unbounded_String ("Target := fake-target"),
         To_Unbounded_String ("fake-ada-fake-target"),
         To_Unbounded_String ("First_Language_Found := ada"));
   begin
      Test ("Common configuration", KB, Descriptions, Expected_Strings);
   end;

   declare
      Descriptions : GPC.Description_Set :=
        (GPC.Create
           (Language => +"Ada",
            Runtime  => "runtime-A",
            Name     => "FAKE-ADA-COMPILER-1"),
         GPC.Create
           (Language => +"C",
            Runtime  => "runtime-A",
            Name     => "FAKE-C-COMPILER-2"));
         Expected_Strings : Unbounded_String_Array (1..0);
   begin
      Test ("Ambiguous path name", KB, Descriptions, Expected_Strings);
   end;

   declare
      Descriptions : GPC.Description_Set :=
        (Positive'First => GPC.Create
           (Language => +"C",
            Runtime  => "runtime-A",
            Name     => "FAKE-C-COMPILER-3"));
      Expected_Strings : Unbounded_String_Array (1..0);
   begin
      Test ("Invalid variable name", KB, Descriptions, Expected_Strings);
   end;

   declare
      Descriptions : GPC.Description_Set :=
        (GPC.Create
           (Language => +"Ada",
            Runtime  => "runtime-A",
            Name     => "FAKE-ADA-COMPILER-2"),
         GPC.Create
           (Language => +"C",
            Runtime  => "runtime-A",
            Name     => "FAKE-C-COMPILER-4"));
         Expected_Strings : Unbounded_String_Array (1..0);
   begin
      Test ("Invalid variable name with *", KB,
            Descriptions, Expected_Strings);
   end;

   declare
      Descriptions : GPC.Description_Set :=
        (GPC.Create
           (Language => +"Ada",
            Runtime  => "runtime-A",
            Name     => "FAKE-ADA-COMPILER-2"),
         GPC.Create
           (Language => +"C",
            Runtime  => "runtime-A",
            Name     => "FAKE-C-COMPILER-5"));
       Expected_Strings : Unbounded_String_Array :=
        (Positive'First => To_Unbounded_String
           ("My_Variable := My_Variable_Value"));
   begin
      Test ("valid variable name with *", KB, Descriptions, Expected_Strings);
   end;

   declare
      Descriptions : GPC.Description_Set :=
        (Positive'First => GPC.Create
           (Language => +"C",
            Runtime  => "runtime-A",
            Name     => "FAKE-C-COMPILER-6"));
      Expected_Strings : Unbounded_String_Array :=
        (Positive'First => To_Unbounded_String
           ("Missing_Variable := "));
   begin
      Test ("Invalid variable name substituted by an empty string", KB,
            Descriptions, Expected_Strings);
   end;
end Main;
