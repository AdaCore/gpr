with Ada.Command_Line;
with Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Text_IO; use Ada.Text_IO;

with GPR2; use GPR2;
with GPR2.Build.Unit_Info;
with GPR2.Containers;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Reporter.Console;

procedure Test is
   use type Ada.Containers.Hash_Type;

   function UTF8 (Code : Natural) return String;

   procedure Check (Left, Right : String; Same : Boolean := True);

   -----------
   -- Check --
   -----------

   procedure Check (Left, Right : String; Same : Boolean := True) is
      L : constant Name_Type := Name_Type (Left);
      R : constant Name_Type := Name_Type (Right);
      Names : Containers.Name_Set;
      LU : constant Build.Unit_Info.Object := Build.Unit_Info.Create
        (L, No_Index, S_Separate, L);
      RU : constant Build.Unit_Info.Object := Build.Unit_Info.Create
        (R, No_Index, S_Separate, R);
      LI : constant Language_Id := +Optional_Name_Type (L);
      RI : constant Language_Id := +Optional_Name_Type (R);
   begin
      if (L = R) /= Same
        or else (To_Lower (L) = To_Lower (R)) /= Same
        or else (To_Upper (L) = To_Upper (R)) /= Same
        or else (LU.Name = RU.Name) /= Same
        or else (LU.Separate_Name = RU.Separate_Name) /= Same
        or else (LI = RI) /= Same
      then
         raise Program_Error with "fold mismatch: " & Left & " / " & Right;
      end if;
      if (L < R) /= (To_Lower (L) < To_Lower (R))
        or else (R < L) /= (To_Lower (R) < To_Lower (L))
        or else (Same and then Hash (L) /= Hash (R))
      then
         raise Program_Error with "order/hash mismatch";
      end if;
      Names.Insert (L);
      if Names.Contains (R) /= Same then
         raise Program_Error with "dependency lookup mismatch";
      end if;
   end Check;

   ----------
   -- UTF8 --
   ----------

   function UTF8 (Code : Natural) return String is
   begin
      return Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode
        (Wide_Wide_String'(1 => Wide_Wide_Character'Val (Code)));
   end UTF8;

   File : File_Type;
begin
   Open (File, In_File, Ada.Command_Line.Argument (1));
   while not End_Of_File (File) loop
      declare
         Line : constant String := Get_Line (File);
         Sep  : constant Positive := Ada.Strings.Fixed.Index (Line, " ");
         Code : constant Natural :=
           Natural'Value ("16#" & Line (1 .. Sep - 1) & '#');
         Folded : constant Natural :=
           Natural'Value ("16#" & Line (Sep + 1 .. Line'Last) & '#');
         Name : constant Name_Type := Name_Type (UTF8 (Code));
      begin
         if To_Lower (Name) /= UTF8 (Folded) then
            raise Program_Error with "Unicode mapping mismatch: " & Line;
         end if;
         Check (String (Name), UTF8 (Folded));
      end;
   end loop;
   Close (File);

   Check (UTF8 (16#03B1#), UTF8 (16#0391#));
   Check (UTF8 (16#03C2#), UTF8 (16#03C3#));
   Check (UTF8 (16#03A3#), UTF8 (16#03C2#));
   Check (UTF8 (16#10400#), UTF8 (16#10428#));
   Check (UTF8 (16#00B5#), UTF8 (16#03BC#));
   Check (UTF8 (16#1E9E#), UTF8 (16#00DF#));
   Check (UTF8 (16#023A#), UTF8 (16#2C65#));
   Check (UTF8 (16#AB70#), UTF8 (16#13A0#));
   Check ("i", UTF8 (16#0131#), False);
   Check ("i", UTF8 (16#0130#), False);
   Check ("ss", UTF8 (16#00DF#), False);
   Check ("ff", UTF8 (16#FB00#), False);
   Check ("a" & UTF8 (16#0301#), UTF8 (16#00E1#), False);
   Check (String'(1 .. 300 => 'A') & UTF8 (16#023A#),
          String'(1 .. 300 => 'a') & UTF8 (16#2C65#));
   Check (String'(1 .. 255 => 'A') & UTF8 (16#0391#),
          String'(1 .. 255 => 'a') & UTF8 (16#03B1#));
   declare
      Slice : String (7 .. 8) := UTF8 (16#0391#);
   begin
      Check (Slice, UTF8 (16#03B1#));
   end;

   if To_Lower (Filename_Optional (UTF8 (16#0391#))) /= UTF8 (16#0391#)
     or else To_Upper_ASCII (UTF8 (16#03B1#)) /= UTF8 (16#03B1#)
     or else To_Lower_ASCII (UTF8 (16#0391#)) /= UTF8 (16#0391#)
   then
      raise Program_Error with "filename/ASCII casing changed";
   end if;
   if Optional_Name_Type'("") /= Optional_Name_Type'("")
     or else Hash (Optional_Name_Type'("")) /= Empty_Hash
     or else To_Lower (No_Name) /= ""
     or else To_Upper (No_Name) /= ""
   then
      raise Program_Error with "empty name mismatch";
   end if;

   declare
      Tree : Project.Tree.Object;
      Opts : Options.Object;
   begin
      Opts.Add_Switch (Options.P, "tree/units.gpr");
      if not Tree.Load (Opts, True, Reporter.Console.Create (Reporter.Quiet))
      then
         raise Program_Error with "project load failed";
      end if;
      Tree.Update_Sources (Sources_Units);
      if not Tree.Root_Project.Own_Unit
        (Name_Type (UTF8 (16#03B1#))).Is_Defined
        or else not Tree.Root_Project.Own_Unit
          (Name_Type (UTF8 (16#03C2#))).Is_Defined
        or else not Tree.Root_Project.Own_Unit
          (Name_Type (UTF8 (16#2C65#))).Is_Defined
      then
         raise Program_Error with "Unicode unit lookup failed";
      end if;
      if not Tree.Root_Project.Has_Specification (UTF8 (16#03B1#))
        or else not Tree.Root_Project.Has_Specification (UTF8 (16#03C2#))
        or else not Tree.Root_Project.Has_Specification (UTF8 (16#2C65#))
      then
         raise Program_Error with "Unicode naming-index lookup failed";
      end if;
   end;
   Put_Line ("OK");
end Test;
