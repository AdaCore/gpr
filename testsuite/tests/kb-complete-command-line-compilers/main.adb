with Ada.Strings.Fixed;
with Ada.Text_IO;
with GPR2.KB;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Configuration;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GPR2; use GPR2;
with GNATCOLL.Traces;

procedure Main is

   package GPC renames GPR2.Project.Configuration;

   type Unbounded_String_Array is
     array (Positive range <>) of Unbounded_String;

   procedure Test
     (Test_Title            : String;
      KB                    : in out GPR2.KB.Object;
      Descriptions          : GPC.Description_Set;
      Target                : Name_Type;
      Fallback              : Boolean;
      Expected_String_Parts : Unbounded_String_Array);
   --  Obtain the configuration for the specified target,
   --  and filters. Display errors and warnings if presents.
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
      Target                : Name_Type;
      Fallback              : Boolean;
      Expected_String_Parts : Unbounded_String_Array)
   is
      Config_Log : Log.Object;
   begin
      declare
         Conf : String := To_String (KB.Configuration
                      (Settings => Descriptions,
                       Target    => Target,
                       Messages  => Config_Log,
                       Fallback  => Fallback));

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

         Ada.Text_IO.Put_Line ("");

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
                    ("fake-compiler-fallback-targets.xml"));
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
         (Positive'First => To_Unbounded_String ("fake-target-0"));
   begin
      Test ("Missing compiler without fallbacks", KB, Descriptions,
            "fake-target-0", False, Expected_Strings);
   end;

   declare
      Descriptions : GPC.Description_Set :=
        (Positive'First => GPC.Create
           (Language => +"Ada",
            Runtime  => "runtime-A",
            Name     => "FAKE-ADA-COMPILER-1"));
      Expected_Strings : Unbounded_String_Array :=
         (Positive'First => To_Unbounded_String ("fake-target-1"));
   begin
      Test ("Missing compiler with fallback", KB, Descriptions,
            "fake-target-0", True, Expected_Strings);
   end;

   declare
      Descriptions : GPC.Description_Set :=
        (Positive'First => GPC.Create
           (Language => +"Unknown_Language"));
      Expected_Strings : Unbounded_String_Array (1..0);
   begin
      Test ("Unknown language", KB, Descriptions,
            "fake-target-1", False, Expected_Strings);
   end;

   declare
      Descriptions : GPC.Description_Set :=
        (1 => GPC.Create
           (Language => +"Ada",
            Runtime  => "runtime-A",
            Name     => "FAKE-ADA-COMPILER-1"),
         2 => GPC.Create
           (Language => +"C",
            Runtime  => "runtime-A",
            Name     => "FAKE-C-COMPILER-1"));
      Expected_Strings : Unbounded_String_Array (1..0);
   begin
      Test ("Incompatible configurations", KB, Descriptions,
            "fake-target-1", True, Expected_Strings);
   end;

   declare
      Descriptions : GPC.Description_Set :=
        (1 => GPC.Create
           (Language => +"Ada",
            Runtime  => "runtime-A"),
         2 => GPC.Create
           (Language => +"C",
            Runtime  => "runtime-A",
            Name     => "FAKE-C-COMPILER-1"));
      Expected_Strings : Unbounded_String_Array (1..0);
   begin
      Test ("Resolvable configuration incompatibility", KB, Descriptions,
            "fake-target-1", True, Expected_Strings);
   end;
end Main;