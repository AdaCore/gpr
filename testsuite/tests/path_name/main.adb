with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Text_IO;    use Ada.Text_IO;

with GNAT.OS_Lib;

with GPR2.Path_Name; use GPR2.Path_Name;

procedure Main is
   First_Check : Boolean := True;

   procedure Check_Path (Header : String;
                         Path   : Object;
                         Short  : Boolean := False)
   is
   begin
      if not First_Check then
         New_Line;
      end if;

      First_Check := False;
      Put_Line (Header);

      if not Path.Is_Defined then
         Put_Line ("- *Undefined*");
         return;
      end if;

      if Path.Is_Directory then
         Put_Line ("- Is a directory");
      else
         Put_Line ("- Is a file");
      end if;

      if Path.Is_Root_Dir then
         Put_Line ("- Root directory");
      end if;

      if Path.Exists then
         Put_Line ("- Actually exists");
      else
         Put_Line ("- Does not exist on disk");
      end if;

      if Short then
         return;
      end if;

      if Path.Has_Dir_Name then
         Put_Line ("- Dir_Name: " & String (Path.Dir_Name));
      end if;

      Put_Line ("- Name: " & String (Path.Name));
      Put_Line ("- Name (no extension): " & String (Path.Name (False)));
      if Path.Has_Value then
         Put_Line ("- Value: " & String (Path.Value));
      end if;

      if not Path.Is_Directory then
         Put_Line ("- Base_Name: " & String (Path.Base_Name));
         Put_Line ("- Base_Filename: " & String (Path.Base_Filename));
      end if;

      if not Path.Is_Root_Dir then
         Put_Line ("- Simple_Name: " & String (Path.Simple_Name));
         Put_Line ("- Containing_Directory: '" & String (Path.Containing_Directory.Name) & "'");
      end if;
   end Check_Path;

   Base       : constant Object := Create_Directory ("files");
   Subdir     : constant Object := Base.Compose ("subdir", True);
   On_Windows : constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';

begin
   Check_Path ("Root dir:", Create_Directory ("/"));
   Check_Path ("'.' dir:", Subdir.Compose (".", True));
   Check_Path ("'..' dir:", Subdir.Compose ("..", True));
   Check_Path ("Non existing file:", Base.Compose ("foo.ext"));
   Check_Path ("existing file:", Subdir.Compose ("file.txt"));
   Check_Path ("File with no extension:", Base.Compose ("foo"));
   Check_Path ("Dir with extension:", Base.Compose ("readme.foo", True));
   Check_Path ("With white space:", Base.Compose ("with white/space file.txt"));
   Check_Path ("Common_Prefix (directory, file)",
               Common_Prefix (Base.Compose ("somedir", True),
                              Subdir.Compose ("foo.txt")));
   Check_Path ("Common_Prefix (file, directory)",
               Common_Prefix (Subdir.Compose ("foo.txt"),
                              Base.Compose ("somedir", True)));
   Check_Path ("Common_Prefix (src, src/source.adb)",
               Common_Prefix (Create_Directory ("src"),
                              Create_File ("src/source.adb")));
   Check_Path ("Common_Prefix (src/source.adb, src)",
               Common_Prefix (Create_File ("src/source.adb"),
                              Create_Directory ("src")));
   Check_Path ("Common_Prefix with root common",
               Common_Prefix (Create_Directory ("/foo/bar"),
                              Create_File ("/bar/foo/test.txt")));
   Check_Path ("File with no full path",
               Create_File ("files/file.txt", No_Resolution));
   Check_Path ("Check temp directory", Temporary_Directory, True);

   --  Additional windows-specific tests
   if On_Windows then
      --  We don't use C:\ because it's filtered out by the testsuite
      --  infrastructure, making the output hard to decipher...
      Check_Path ("Root dir D:", Create_Directory ("D:\"));
      Check_Path ("Common_Prefix (D:\foo\, E:\bar\)",
                  Common_Prefix (Create_Directory ("D:\foo\"),
                                 Create_Directory ("E:\bar\")));
      Check_Path ("Relative_Path (D:\foo\, E:\bar\)",
                  Relative_Path (Create_Directory ("D:\foo\"),
                                 Create_Directory ("E:\bar\")));
      Check_Path ("Relative_Path (D:\Foo\Bar\, D:\foo\baz\)",
                  Relative_Path (Create_Directory ("D:\Foo\Bar\"),
                                 Create_Directory ("D:\foo\baz\")));
   end if;
end Main;
