with Ada.Directories;
with Ada.Text_IO;

with GNATCOLL.OS.Constants; use GNATCOLL.OS.Constants;

with GPR2.Path_Name;

procedure Main is

   use Ada;
   use GPR2;

   P1 : constant Path_Name.Object :=
          Path_Name.Create_File ("/dir1/dir2/toto");
   P2 : constant Path_Name.Object :=
          Path_Name.Create_File ("/dir1/dir2/dir3/toto");
   P3 : constant Path_Name.Object :=
          Path_Name.Create_File ("/dira/toto");
   P4 : constant Path_Name.Object :=
          Path_Name.Create_Directory ("/dira/dirb/dirc");
   P5 : constant Path_Name.Object :=
          Path_Name.Create_Directory ("/dir1/dir2/toto");
   P6 : constant Path_Name.Object :=
          Path_Name.Create_File ("/dir1/dir2/toto/file");
   C1 : constant Path_Name.Object := Path_Name.Create_File ("project.gpr");
   C2 : constant Path_Name.Object := Path_Name.Create_File ("subdir/one.gpr");

   PV : Path_Name.Object;

   FH : constant GPR2.Path_Name.Object :=
          GPR2.Path_Name.Create_Directory
            ("/gnatmail/gpr2/x86-windows/gpr2/install/share/gpr");
   TH : constant GPR2.Path_Name.Object :=
          GPR2.Path_Name.Create_Directory
            ("/gnatmail/gpr2/x86-windows/gpr2-test/src/testsuite/tests/m");

   W1 : constant Path_Name.Object :=
          Path_Name.Create_Directory ("C:\dir1\dir2\toto");
   W2 : constant Path_Name.Object :=
          Path_Name.Create_Directory ("c:\Dir1\Dir2\toto");
   W3 : constant Path_Name.Object :=
          Path_Name.Create_Directory ("D:\dir1\dir2\toto");

   procedure Assert (Actual, Expected : String);

   ------------
   -- Assert --
   ------------

   procedure Assert (Actual, Expected : String) is
   begin
      if Expected /= Actual then
         Text_IO.Put_Line ("Expected: " & Expected & " Actual: " & Actual);
      end if;
   end Assert;

begin
   Text_IO.Put_Line ("1: " & String (Path_Name.Relative_Path (P1, P2)));
   Text_IO.Put_Line ("2: " & String (Path_Name.Relative_Path (P3, P2)));
   Text_IO.Put_Line ("3: " & String (Path_Name.Relative_Path (P2, P1)));
   Text_IO.Put_Line ("4: " & String (Path_Name.Relative_Path (P3, P4)));
   Text_IO.Put_Line ("5: " & String (Path_Name.Relative_Path (P4, P3)));
   Text_IO.Put_Line ("6: " & String (Path_Name.Relative_Path (P1, P1)));
   Text_IO.Put_Line ("7: " & String (Path_Name.Relative_Path (P4, P4)));
   Text_IO.Put_Line ("8: " & String (C2.Relative_Path (C1)));
   Text_IO.Put_Line ("9: " & String (C1.Relative_Path (C2)));
   Text_IO.Put_Line ("A: " & String (Path_Name.Relative_Path (P6, P5)));

   --  on windows current drive is added to absolute GPR2.Path_Name.
   --  use Ada.Directories.Full_Name to get normalized path.
   --  /dir1/dir2 on linux, C:/dir1/dir2 on windows

   Assert
     (P5.Containing_Directory.String_Value, Directories.Full_Name ("/dir1/dir2"));

   PV := C2.Change_Extension ("");
   Text_IO.Put_Line ("B: " & String (PV.Name));
   PV := C2.Change_Extension (".");
   Text_IO.Put_Line ("C: " & String (PV.Name));
   PV := PV.Change_Extension ("a");
   Text_IO.Put_Line ("D: " & String (PV.Name));
   PV := PV.Change_Extension ("bcd");
   Text_IO.Put_Line ("E: " & String (PV.Name));
   PV := PV.Change_Extension ("ef");
   Text_IO.Put_Line ("F: " & String (PV.Name));
   PV := PV.Change_Extension ("ef");
   Text_IO.Put_Line ("G: " & String (PV.Name));
   Text_IO.Put_Line ("H: " & String (Path_Name.Relative_Path (FH, TH)));

   if On_Windows then
      Assert (String (W1.Relative_Path (W2)), "." & Dir_Sep);
      Assert (String (W1.Relative_Path (W3)), W1.String_Value);
   end if;
end Main;